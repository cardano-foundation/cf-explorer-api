package org.cardanofoundation.explorer.api.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

@Service
@AllArgsConstructor
@Log4j2
public class ScriptServiceImpl implements ScriptService {

  private ScriptRepository scriptRepository;
  private TxOutRepository txOutRepository;
  private StakeAddressRepository stakeAddressRepository;
  private RedeemerRepository redeemerRepository;
  private TxRepository txRepository;

  private ScriptMapper scriptMapper;

  @Override
  public SmartContractDetailResponse getSmartContractDetail(String scriptHash) {
    Script script = scriptRepository.findByHash(scriptHash).orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
    );

    if(!script.getType().equals(ScriptType.PLUTUSV1) && !script.getType().equals(ScriptType.PLUTUSV2)) {
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }

    List<String> associatedAddresses =
        Stream.concat(stakeAddressRepository.getAssociatedAddress(scriptHash).stream(),
                      txOutRepository.getAssociatedAddress(scriptHash).stream())
            .toList();

    return SmartContractDetailResponse.builder()
        .scriptHash(script.getHash())
        .scriptType(script.getType())
        .associatedAddresses(associatedAddresses)
        .build();
  }

  @Override
  public BaseFilterResponse<SmartContractTxResponse> getSmartContractTxs(String scriptHash,
                                                                         Pageable pageable) {

    Page<Long> txIds = redeemerRepository.findTxIdsInteractWithContract(scriptHash, pageable);
    // get smart contract tx map
    Map<Long, SmartContractTxResponse> smartContractTxMap =
        txRepository.getSmartContractTxsByTxIds(txIds.getContent())
            .stream()
            .map(scriptMapper::fromSmartContractTxProjection)
            .collect(Collectors.toMap(SmartContractTxResponse::getTxId, Function.identity()));

    // get address input map
    Map<Long, List<AddressInputOutputProjection>> addressInMap =
        txOutRepository.findAddressInputListByTxId(txIds.getContent())
            .stream()
            .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    // get address output map
    Map<Long, List<AddressInputOutputProjection>> addressOutMap =
        txOutRepository.findAddressOutputListByTxId(txIds.getContent())
            .stream()
            .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    // get script purpose type map
    Map<Long, List<ScriptPurposeType>> scriptPurposeTypeMap =
        txRepository.getSmartContractTxsPurpose(txIds.getContent(), scriptHash)
            .stream()
            .collect(Collectors.groupingBy(SmartContractTxProjection::getTxId))
            .entrySet().stream()
            .collect(Collectors.toMap(Map.Entry::getKey,
                                      e -> e.getValue().stream()
                                          .map(SmartContractTxProjection::getScriptPurposeType)
                                          .toList()));

    List<SmartContractTxResponse> smartContractTxResponses = new ArrayList<>();
    txIds.stream().forEach(txId -> {
      SmartContractTxResponse smartContractTxResponse = smartContractTxMap.get(txId);
      List<String> inputAddressList = addressInMap.getOrDefault(smartContractTxResponse.getTxId(),
                                                                new ArrayList<>())
          .stream()
          .map(AddressInputOutputProjection::getAddress)
          .toList();

      List<String> outputAddressList = addressOutMap.getOrDefault(smartContractTxResponse.getTxId(),
                                                                  new ArrayList<>())
          .stream()
          .map(AddressInputOutputProjection::getAddress)
          .toList();

      smartContractTxResponse.setAddresses(
          Stream.concat(inputAddressList.stream(), outputAddressList.stream())
              .collect(Collectors.toSet()));
      smartContractTxResponse.setScriptPurposeTypes(
          scriptPurposeTypeMap.get(smartContractTxResponse.getTxId()));

      smartContractTxResponses.add(smartContractTxResponse);
    });

    return new BaseFilterResponse<>(txIds, smartContractTxResponses);
  }

  @Override
  public ScriptSearchResponse searchScript(String scriptHash) {
    Script script = scriptRepository.findByHash(scriptHash).orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
    );
    boolean isSmartContract = ScriptType.PLUTUSV1.equals(script.getType()) ||
        ScriptType.PLUTUSV2.equals(script.getType());
    ScriptSearchResponse scriptSearchResponse = ScriptSearchResponse.builder()
        .scriptHash(script.getHash())
        .build();

    if (isSmartContract) {
      scriptSearchResponse.setSmartContract(true);
    } else {
      scriptSearchResponse.setNativeScript(true);
    }
    return scriptSearchResponse;
  }
}
