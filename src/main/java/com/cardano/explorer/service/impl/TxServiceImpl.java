package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.entity.projection.AddressInputOutput;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.exception.BusinessException;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.TxFilterResponse;
import com.cardano.explorer.model.TxResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.TxService;
import com.sotatek.cardano.ledgersync.util.HexUtil;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getAll(Pageable pageable) {
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Page<Tx> txPage = txRepository.findAll(pageable);

    // get block information for each transaction
    List<Long> blockIdList = txPage.getContent().stream().map(Tx::getBlockId)
        .collect(Collectors.toList());
    List<Block> blocks = blockRepository.findBlockByIdIn(blockIdList);
    Map<Long, Block> mapBlock = blocks.stream()
        .collect(Collectors.toMap(Block::getId, Function.identity()));
    txPage.getContent().forEach(tx -> tx.setBlock(mapBlock.get(tx.getBlockId())));


    List<TxFilterResponse> txFilterResponses = mapDataFromTxToResponse(txPage.getContent());

    response.setData(txFilterResponses);
    response.setCurrentPage(pageable.getPageNumber());
    response.setTotalPages(txPage.getTotalPages());
    response.setTotalItems(txPage.getTotalElements());
    return response;
  }

  /**
   * Mapping from tx entity list to tx response dto
   *
   * @param txList list tx entity
   * @return list tx response
   */
  private List<TxFilterResponse> mapDataFromTxToResponse(List<Tx> txList) {

    //get addresses input
    Set<Long> txIdSet = txList.stream().map(Tx::getId).collect(Collectors.toSet());
    List<AddressInputOutput> txInList = txOutRepository.findAddressInputByTxIdIn(txIdSet);
    Map<Long, List<AddressInputOutput>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutput::getTxId));

    //get addresses output
    List<AddressInputOutput> txOutList = txOutRepository.findByTxInOrderByIndexAsc(
        txList);
    Map<Long, List<AddressInputOutput>> addressOutMap = txOutList.stream()
        .collect(Collectors.groupingBy(AddressInputOutput::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (Tx tx : txList) {
      Long txId = tx.getId();
      TxFilterResponse txResponse = txMapper.txToTxFilterResponse(tx);
      if (addressOutMap.containsKey(txId)) {
        txResponse.setAddressesOutput(
            addressOutMap.get(tx.getId()).stream().map(AddressInputOutput::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesOutput(new ArrayList<>());
      }
      if (addressInMap.containsKey(txId)) {
        txResponse.setAddressesInput(
            addressInMap.get(tx.getId()).stream().map(AddressInputOutput::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesInput(new ArrayList<>());
      }
      txFilterResponses.add(txResponse);
    }
    return txFilterResponses;
  }

  @Override
  @Transactional(readOnly = true)
  public TxResponse getTxDetailByHash(String hash) {
    Tx tx = txRepository.findByHash(HexUtil.decodeHexString(hash)).orElseThrow(
        () -> new BusinessException(BusinessCode.NOT_FOUND)
    );
    Integer currentBlockNo = blockRepository.findCurrentBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.NOT_FOUND)
    );
    TxResponse txResponse = txMapper.txToTxResponse(tx);
    txResponse.setConfirmation(currentBlockNo - txResponse.getBlockNo());
    txResponse.setStatus(TxStatus.SUCCESS);
    return txResponse;
  }
}
