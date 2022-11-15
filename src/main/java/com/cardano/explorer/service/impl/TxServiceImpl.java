package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.entity.projection.AddressInputOutput;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.mapper.TxOutMapper;
import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxOutResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.TxService;
import com.cardano.explorer.specification.BlockSpecification;
import com.cardano.explorer.specification.TxSpecification;
import com.sotatek.cardano.ledgersync.util.HexUtil;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;

  private final TxOutMapper txOutMapper;

  private final TxSpecification txSpecification;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> filterTx(Pageable pageable, TxFilterRequest request) {

    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Page<Tx> txPage;
    if (request != null) {
      txPage = txRepository.findAll(txSpecification.getFilter(request), pageable);
    } else {
      txPage = txRepository.findAll(pageable);
    }
    Set<Long> blockIdList = txPage.getContent().stream().map(Tx::getBlockId)
        .collect(Collectors.toSet());
    var conditions = Specification.where(BlockSpecification.hasIdIn(blockIdList));
    List<Block> blocks = blockRepository.findAll(conditions);
    Map<Long, Block> mapBlock = blocks.stream()
        .collect(Collectors.toMap(Block::getId, Function.identity()));
    txPage.getContent().forEach(tx -> tx.setBlock(mapBlock.get(tx.getBlockId())));

    List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage.getContent());
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
  private List<TxFilterResponse> mapDataFromTxListToResponseList(List<Tx> txList) {

    //get addresses input
    Set<Long> txIdSet = txList.stream().map(Tx::getId).collect(Collectors.toSet());
    List<AddressInputOutput> txInList = txOutRepository.findAddressInputListByTxId(txIdSet);
    Map<Long, List<AddressInputOutput>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutput::getTxId));

    //get addresses output
    List<AddressInputOutput> txOutList = txOutRepository.findAddressOutputListByTxId(txIdSet);
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
    byte[] txHash = HexUtil.decodeHexString(hash);
    Tx tx = txRepository.findByHash(
        txHash).orElseThrow(
        () -> new BusinessException(BusinessCode.TRANSACTION_NOT_FOUND)
    );
    Integer currentBlockNo = blockRepository.findCurrentBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    TxResponse txResponse = txMapper.txToTxResponse(tx);
    txResponse.setConfirmation(currentBlockNo - txResponse.getBlockNo());
    txResponse.setStatus(TxStatus.SUCCESS);

    // get address input output
    List<AddressInputOutput> addressInputInfo = txOutRepository.getTxAddressInputInfo(txHash);
    List<AddressInputOutput> addressOutputInfo = txOutRepository.getTxAddressOutputInfo(txHash);

    txResponse.setUtxOInputList(addressInputInfo.stream().map(txOutMapper::fromAddressInputOutput).collect(
        Collectors.toList()));
    txResponse.setUtxOOutputList(addressOutputInfo.stream().map(txOutMapper::fromAddressInputOutput).collect(
        Collectors.toList()));

    txResponse.setStakeAddressTxInputList(getStakeAddressInfo(addressInputInfo));
    txResponse.setStakeAddressTxOutputList(getStakeAddressInfo(addressOutputInfo));

    return txResponse;
  }

  /**
   * Get stake address info from address
   *
   * @param addressInputOutputList List address input or output info
   * @return list stake address input or ouput info
   */
  private static List<TxOutResponse> getStakeAddressInfo(List<AddressInputOutput> addressInputOutputList) {
    var addressInputMap = addressInputOutputList.stream().collect(Collectors.groupingBy(
        AddressInputOutput::getStakeAddress,
        Collectors.reducing(BigDecimal.ZERO, AddressInputOutput::getValue, BigDecimal::add)
    ));
    List<TxOutResponse> stakeAddressTxInputList = new ArrayList<>();
    addressInputMap.forEach(
        (key, value) -> stakeAddressTxInputList.add(TxOutResponse.builder()
          .address(key)
          .value(value)
          .build())
    );
    return stakeAddressTxInputList;
  }
}
