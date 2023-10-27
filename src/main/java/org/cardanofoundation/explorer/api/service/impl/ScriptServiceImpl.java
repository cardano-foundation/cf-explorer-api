package org.cardanofoundation.explorer.api.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.TxOutRepository;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
@Log4j2
public class ScriptServiceImpl implements ScriptService {

  private ScriptRepository scriptRepository;
  private MultiAssetRepository multiAssetRepository;
  private TxOutRepository txOutRepository;
  private StakeAddressRepository stakeAddressRepository;

  @Override
  public BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(Pageable pageable) {
    // Native script is a script that is a timelock script
    List<ScriptType> nativeScriptTypes = List.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    Page<Script> nativeScripts = scriptRepository.findAllByTypeIn(nativeScriptTypes, pageable);
    List<String> scriptHashList = nativeScripts.getContent().stream().map(Script::getHash).toList();
    List<PolicyProjection> numberOfTokenList = multiAssetRepository.countMultiAssetByPolicyIn(scriptHashList);
    Map<String, Integer> numberOfTokenMap = numberOfTokenList.stream()
        .collect(Collectors.toMap(PolicyProjection::getPolicy, PolicyProjection::getNumberOfTokens));
    List<PolicyProjection> numberOfAssetHolderList =
        multiAssetRepository.countAssetHoldersByPolicyIn(scriptHashList);
    Map<String, Integer> numberOfAssetHolderMap = numberOfAssetHolderList.stream()
        .collect(Collectors.toMap(PolicyProjection::getPolicy, PolicyProjection::getNumberOfAssetHolders));
    Page<NativeScriptFilterResponse> nativeScriptPage = nativeScripts.map(
        script ->  {
          Integer numberOfTokens = numberOfTokenMap.get(script.getHash());
          if (Objects.isNull(numberOfTokens)) {
            numberOfTokens = 0;
          }
          Integer numberOfAssetHolders = numberOfAssetHolderMap.get(script.getHash());
          if (Objects.isNull(numberOfAssetHolders)) {
            numberOfAssetHolders = 0;
          }
          return NativeScriptFilterResponse.builder()
              .scriptHash(script.getHash())
              .numberOfTokens(numberOfTokens)
              .numberOfAssetHolders(numberOfAssetHolders)
              .build();
        });
    return new BaseFilterResponse<>(nativeScriptPage);
  }

  @Override
  public BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(Pageable pageable) {
    List<ScriptType> smartContractTypes = List.of(ScriptType.PLUTUSV1, ScriptType.PLUTUSV2);
    Page<Script> smartContracts = scriptRepository.findAllByTypeIn(smartContractTypes, pageable);
    List<String> scriptHashList = smartContracts.getContent().stream().map(Script::getHash).toList();
    List<SmartContractProjection> paymentAddressList =
        txOutRepository.findPaymentAssociatedAddressByHashIn(scriptHashList);
    Map<String, List<String>> paymentAddressMap = paymentAddressList.stream()
        .collect(Collectors.groupingBy(
            SmartContractProjection::getScriptHash,
            Collectors.mapping(SmartContractProjection::getAddress, Collectors.toList())));
    List<SmartContractProjection> stakeAddressList =
        stakeAddressRepository.findStakeAssociatedAddressByHashIn(scriptHashList);
    Map<String, List<String>> stakeAddressMap = stakeAddressList.stream()
        .collect(Collectors.groupingBy(
            SmartContractProjection::getScriptHash,
            Collectors.mapping(SmartContractProjection::getAddress, Collectors.toList())));
    Page<SmartContractFilterResponse> smartContractPage = smartContracts.map(
        script ->  {
          List<String> stakeAddress = stakeAddressMap.get(script.getHash());
          if (Objects.isNull(stakeAddress)) {
            stakeAddress = new ArrayList<>();
          }
          List<String> paymentAddress = paymentAddressMap.get(script.getHash());
          if (Objects.isNull(paymentAddress)) {
            paymentAddress = new ArrayList<>();
          }
          stakeAddress.addAll(paymentAddress);
          return SmartContractFilterResponse.builder()
              .scriptHash(script.getHash())
              .version(script.getType())
              .associatedAddress(stakeAddress)
              .build();
        });
    return new BaseFilterResponse<>(smartContractPage);
  }
}
