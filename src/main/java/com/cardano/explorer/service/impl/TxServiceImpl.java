package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.mapper.TxOutMapper;
import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.tx.CollateralResponse;
import com.cardano.explorer.model.response.tx.TxOutResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.cardano.explorer.model.response.tx.ContractResponse;
import com.cardano.explorer.model.response.tx.SummaryResponse;
import com.cardano.explorer.model.response.tx.UTxOResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.cardano.explorer.projection.CollateralInputOutputProjection;
import com.cardano.explorer.projection.TxContract;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.CollateralTxInRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.RedeemerRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.TxService;
import com.cardano.explorer.specification.BlockSpecification;
import com.cardano.explorer.specification.TxSpecification;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.enumeration.ScriptPurposeType;
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
import org.springframework.util.CollectionUtils;

@Service
@RequiredArgsConstructor
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;
  private final TxOutMapper txOutMapper;
  private final TxSpecification txSpecification;
  private final RedeemerRepository redeemerRepository;

  private final EpochRepository epochRepository;

  private final CollateralTxInRepository collateralTxInRepository;

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
    List<AddressInputOutputProjection> txInList = txOutRepository.findAddressInputListByTxId(txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    //get addresses output
    List<AddressInputOutputProjection> txOutList = txOutRepository.findAddressOutputListByTxId(txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressOutMap = txOutList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (Tx tx : txList) {
      Long txId = tx.getId();
      TxFilterResponse txResponse = txMapper.txToTxFilterResponse(tx);
      if (addressOutMap.containsKey(txId)) {
        txResponse.setAddressesOutput(
            addressOutMap.get(tx.getId()).stream().map(AddressInputOutputProjection::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesOutput(new ArrayList<>());
      }
      if (addressInMap.containsKey(txId)) {
        txResponse.setAddressesInput(
            addressInMap.get(tx.getId()).stream().map(AddressInputOutputProjection::getAddress)
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
    Tx tx = txRepository.findByHash(hash).orElseThrow(
        () -> new BusinessException(BusinessCode.TRANSACTION_NOT_FOUND)
    );
    Integer currentBlockNo = blockRepository.findCurrentBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    TxResponse txResponse = txMapper.txToTxResponse(tx);
    // TO DO
    /*Epoch epoch = epochRepository.findByNo(txResponse.getTx().getEpochNo()).orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
    );*/
    txResponse.getTx().setMaxEpochSlot(432000);
    txResponse.getTx().setConfirmation(currentBlockNo - txResponse.getTx().getBlockNo());
    txResponse.getTx().setStatus(TxStatus.SUCCESS);

    // get address input output
    getSummaryAndUTxOs(hash, txResponse);
    getContracts(tx, txResponse);
    getCollaterals(tx, txResponse);
    return txResponse;
  }

  /**
   * Get transaction collaterals info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getCollaterals(Tx tx, TxResponse txResponse) {
    List<CollateralInputOutputProjection> collateralInputs = collateralTxInRepository.findTxCollateralInput(
        tx);
    List<CollateralResponse> collateralInputResponses = collateralInputs.stream().map(
        collateralInputOutputProjection -> CollateralResponse.builder()
            .txHash(collateralInputOutputProjection.getTxHash())
            .amount(collateralInputOutputProjection.getValue())
            .address(collateralInputOutputProjection.getAddress())
            .build()
    ).collect(Collectors.toList());
    if(!CollectionUtils.isEmpty(collateralInputResponses)) {
      txResponse.setCollaterals(collateralInputResponses);
    }
  }

  /**
   * Get transaction contracts info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getContracts(Tx tx, TxResponse txResponse) {
    List<TxContract> redeemers = redeemerRepository.findContractByTx(tx);
    List<ContractResponse> contractResponses = redeemers.stream().map(redeemer -> {
      if(redeemer.getPurpose().equals(ScriptPurposeType.SPEND)) {
        return new ContractResponse(redeemer.getAddress());
      } else {
        return new ContractResponse(redeemer.getScriptHash());
      }
    }).collect(Collectors.toList());
    if(!CollectionUtils.isEmpty(contractResponses)) {
      txResponse.setContracts(contractResponses);
    }
  }

  /**
   * Get transaction summary and UTxOs info
   *
   * @param hash hash value of transaction
   * @param txResponse response data of transaction
   */
  private void getSummaryAndUTxOs(String hash, TxResponse txResponse) {
    List<AddressInputOutputProjection> addressInputInfo = txOutRepository.getTxAddressInputInfo(hash);
    List<AddressInputOutputProjection> addressOutputInfo = txOutRepository.getTxAddressOutputInfo(hash);
    if(!CollectionUtils.isEmpty(addressInputInfo) && !CollectionUtils.isEmpty(addressOutputInfo)) {
      UTxOResponse uTxOs = UTxOResponse.builder()
          .inputs(addressInputInfo.stream().map(txOutMapper::fromAddressInputOutput).collect(
              Collectors.toList()))
          .outputs(addressOutputInfo.stream().map(txOutMapper::fromAddressInputOutput).collect(
              Collectors.toList()))
          .build();
      txResponse.setUTxOs(uTxOs);
      SummaryResponse summary = SummaryResponse.builder()
          .stakeAddressTxInputs(getStakeAddressInfo(addressInputInfo))
          .stakeAddressTxOutputs(getStakeAddressInfo(addressOutputInfo))
          .build();
      txResponse.setSummary(summary);
    }

  }

  /**
   * Get stake address info from address
   *
   * @param addressInputOutputProjectionList List address input or output info
   * @return list stake address input or output info
   */
  private static List<TxOutResponse> getStakeAddressInfo(List<AddressInputOutputProjection> addressInputOutputProjectionList) {
    var addressInputMap = addressInputOutputProjectionList.stream().collect(Collectors.groupingBy(
        AddressInputOutputProjection::getStakeAddress,
        Collectors.reducing(BigDecimal.ZERO, AddressInputOutputProjection::getValue, BigDecimal::add)
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
