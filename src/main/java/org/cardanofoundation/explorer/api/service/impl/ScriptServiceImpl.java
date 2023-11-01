package org.cardanofoundation.explorer.api.service.impl;

import com.bloxbean.cardano.client.exception.CborDeserializationException;
import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.entity.*;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.cardanofoundation.ledgersync.common.common.nativescript.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@AllArgsConstructor
@Log4j2
public class ScriptServiceImpl implements ScriptService {

  private final ScriptRepository scriptRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final TxOutRepository txOutRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressRepository addressRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final BlockRepository blockRepository;
  private final RedeemerRepository redeemerRepository;
  private final TxRepository txRepository;

  private final AssetMetadataMapper assetMetadataMapper;
  private final TokenMapper tokenMapper;
  private final ScriptMapper scriptMapper;

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

  /**
   * Explain native script
   * @param nativeScript native script
   * @param nativeScriptResponse native script response
   * @param firstBlockTime time blockchain started
   */
  private void setNativeScriptInfo(NativeScript nativeScript,
                                   NativeScriptResponse nativeScriptResponse,
                                   Timestamp firstBlockTime) {
    if (nativeScript.getClass().equals(ScriptPubkey.class)) {
      ScriptPubkey scriptPubkey = (ScriptPubkey) nativeScript;
      nativeScriptResponse.getKeyHashes().add(scriptPubkey.getKeyHash());
    } else if (nativeScript.getClass().equals(ScriptAll.class)) {
      ScriptAll scriptAll = (ScriptAll) nativeScript;
      nativeScriptResponse.setConditionType(scriptAll.getType());
      for (NativeScript script : scriptAll.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse, firstBlockTime);
      }
    } else if (nativeScript.getClass().equals(ScriptAny.class)) {
      ScriptAny scriptAny = (ScriptAny) nativeScript;
      nativeScriptResponse.setConditionType(scriptAny.getType());
      for (NativeScript script : scriptAny.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse, firstBlockTime);
      }
    } else if (nativeScript.getClass().equals(ScriptAtLeast.class)) {
      ScriptAtLeast scriptAtLeast = (ScriptAtLeast) nativeScript;
      nativeScriptResponse.setConditionType(scriptAtLeast.getType());
      nativeScriptResponse.setRequired(scriptAtLeast.getRequired());
      for (NativeScript script : scriptAtLeast.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse, firstBlockTime);
      }
    } else if (nativeScript.getClass().equals(RequireTimeAfter.class)) {
      RequireTimeAfter requireTimeAfter = (RequireTimeAfter) nativeScript;
      LocalDateTime after = firstBlockTime
          .toLocalDateTime()
          .plusSeconds(requireTimeAfter.getSlot().longValue());
      nativeScriptResponse.setAfter(after);
    } else if (nativeScript.getClass().equals(RequireTimeBefore.class)) {
      RequireTimeBefore requireTimeBefore = (RequireTimeBefore) nativeScript;
      LocalDateTime before = firstBlockTime
          .toLocalDateTime()
          .plusSeconds(requireTimeBefore.getSlot().longValue());
      nativeScriptResponse.setBefore(before);
    }
  }

  @Override
  public NativeScriptResponse getNativeScripts(String scriptHash) {
    NativeScriptResponse nativeScriptResponse = new NativeScriptResponse();
    Script script = scriptRepository.findByHash(scriptHash).orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
    );
    Set<ScriptType> nativeScriptTypes = Set.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    if (!nativeScriptTypes.contains(script.getType())) {
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }
    nativeScriptResponse.setScriptHash(scriptHash);
    Block block = blockRepository.findFirstBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND));
    List<String> associatedAddressList = stakeAddressRepository.getStakeAssociatedAddress(scriptHash);
    associatedAddressList.addAll(txOutRepository.getAssociatedAddress(scriptHash));
    nativeScriptResponse.setAssociatedAddress(associatedAddressList);
    nativeScriptResponse.setKeyHashes(new ArrayList<>());
    try {
      String json = script.getJson();
      if (!StringUtils.isEmpty(json)) {
        if (Boolean.TRUE.equals(script.getVerifiedContract())) {
          nativeScriptResponse.setScript(json);
          NativeScript nativeScript = NativeScript.deserializeJson(json);
          setNativeScriptInfo(nativeScript, nativeScriptResponse, block.getTime());
        }
        nativeScriptResponse.setVerifiedContract(script.getVerifiedContract());
      }
    } catch (JsonProcessingException | CborDeserializationException e) {
      log.warn("Error parsing script json: {}", e.getMessage());
    }
    return nativeScriptResponse;
  }


  @Override
  @Transactional
  public String verifyNativeScript(String scriptHash, String scriptJson) {
    try {
      Script script = scriptRepository.findByHash(scriptHash).orElseThrow(
          () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
      );
      Set<ScriptType> nativeScriptTypes = Set.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
      if (!nativeScriptTypes.contains(script.getType())) {
        throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
      }
      String hash = Hex.encodeHexString(NativeScript.deserializeJson(scriptJson).getScriptHash());
      if (script.getHash().equals(hash)) {
        script.setVerifiedContract(true);
        scriptRepository.save(script);
        return script.getJson();
      } else {
        throw new BusinessException(BusinessCode.VERIFY_SCRIPT_FAILED);
      }
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.VERIFY_SCRIPT_FAILED);
    }
  }


  @Override
  public BaseFilterResponse<TokenFilterResponse> getNativeScriptTokens(String scriptHash, Pageable pageable) {
    Page<MultiAsset> multiAssetPage = multiAssetRepository.findAllByPolicy(scriptHash, pageable);
    Set<String> subjects = multiAssetPage.stream().map(
        ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    var multiAssetResponsesList = multiAssetPage.map(tokenMapper::fromMultiAssetToFilterResponse);
    multiAssetResponsesList.forEach(
        ma -> ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
            assetMetadataMap.get(ma.getPolicy() + ma.getName()))
        )
    );
    return new BaseFilterResponse<>(multiAssetResponsesList);
  }

  @Override
  public BaseFilterResponse<TokenAddressResponse> getNativeScriptHolders(String scriptHash, Pageable pageable) {
    Page<AddressTokenProjection> multiAssetPage
        = addressTokenBalanceRepository.findAddressAndBalanceByMultiAssetIn(scriptHash, pageable);
    Set<Long> addressIds = multiAssetPage.stream().map(AddressTokenProjection::getAddressId)
        .collect(Collectors.toSet());
    List<Address> addressList = addressRepository.findAddressByIdIn(addressIds);
    Map<Long, Address> addressMap = addressList.stream().collect(
        Collectors.toMap(Address::getId, Function.identity()));
    Page<TokenAddressResponse> tokenAddressResponses = multiAssetPage.map(
        tokenMapper::fromAddressTokenProjection);
    Set<String> subjects = multiAssetPage.stream().map(
        ma -> ma.getPolicy() + ma.getTokenName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    tokenAddressResponses.forEach(tokenAddress -> {
      tokenAddress.setAddress(
          addressMap.get(tokenAddress.getAddressId()).getAddress());
      tokenAddress.setMetadata(assetMetadataMapper.fromAssetMetadata(
          assetMetadataMap.get(tokenAddress.getPolicy() + tokenAddress.getName())));
      tokenAddress.setAddressId(null);
    });
    return new BaseFilterResponse<>(tokenAddressResponses);
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
}
