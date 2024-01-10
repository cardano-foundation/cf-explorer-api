package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.bloxbean.cardano.client.exception.CborDeserializationException;
import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.explorer.NativeScriptInfoRepository;
import org.cardanofoundation.explorer.api.repository.explorer.SmartContractInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.specification.NativeScriptInfoSpecification;
import org.cardanofoundation.explorer.consumercommon.entity.*;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.NativeScriptInfo;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.SmartContractInfo;
import org.cardanofoundation.ledgersync.common.common.nativescript.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Service
@RequiredArgsConstructor
@Log4j2
public class ScriptServiceImpl implements ScriptService {

  private final ScriptRepository scriptRepository;
  private final NativeScriptInfoRepository nativeScriptInfoRepository;
  private final TxOutRepository txOutRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RedeemerRepository redeemerRepository;
  private final TxRepository txRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressRepository addressRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final BlockRepository blockRepository;
  private final SmartContractInfoRepository smartContractInfoRepository;

  private final TxService txService;

  private final ScriptMapper scriptMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final TokenMapper tokenMapper;

  private Block firstShellyBlock = null;
  private Block firstBlock = null;

  private static final Long MAX_SLOT = 365241780471L;

  @PostConstruct
  private void init() {
    firstShellyBlock = blockRepository.findFirstShellyBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    firstBlock = blockRepository.findFirstBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
  }


  @Override
  public BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(NativeScriptFilterRequest filterRequest,
                                                                         Pageable pageable) {
    Block currrentBlock = blockRepository.findLatestBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    Page<NativeScriptInfo> nativeScriptPage = nativeScriptInfoRepository.findAll(
        NativeScriptInfoSpecification.filter(currrentBlock.getSlotNo(), filterRequest), pageable);
    List<MultiAsset> multiAssetList = multiAssetRepository.findTopMultiAssetByScriptHashIn(
        nativeScriptPage.stream().map(NativeScriptInfo::getScriptHash).toList());
    List<TokenFilterResponse> tokenResponses = createTokenResponse(multiAssetList);
    Map<String, List<TokenFilterResponse>> tokenResponseMap =
        tokenResponses.stream().collect(Collectors.groupingBy(TokenFilterResponse::getPolicy));
    Page<NativeScriptFilterResponse> nativeScriptPageResponse =
        nativeScriptPage.map(item -> {
          NativeScriptFilterResponse nativeScriptResponse = new NativeScriptFilterResponse();
          nativeScriptResponse.setScriptHash(item.getScriptHash());
          nativeScriptResponse.setNumberOfTokens(item.getNumberOfTokens());
          nativeScriptResponse.setNumberOfAssetHolders(item.getNumberOfAssetHolders());
          if (Objects.nonNull(item.getAfterSlot())) {
            nativeScriptResponse.setAfter(slotToTime(item.getAfterSlot(), firstBlock, firstShellyBlock));
          }
          if (Objects.nonNull(item.getBeforeSlot())) {
            nativeScriptResponse.setBefore(slotToTime(item.getBeforeSlot(), firstBlock, firstShellyBlock));
          }
          if (Objects.nonNull(item.getNumberSig())) {
            nativeScriptResponse.setIsMultiSig(Long.valueOf(1L).compareTo(item.getNumberSig()) < 0);
          }
          nativeScriptResponse.setTokens(tokenResponseMap.get(item.getScriptHash()));
          return nativeScriptResponse;
        });
    return new BaseFilterResponse<>(nativeScriptPageResponse);
  }

  private List<TokenFilterResponse> createTokenResponse(List<MultiAsset> tokens) {
    List<TokenFilterResponse> tokenResponses;
    Set<String> subjects = tokens.stream().map(
        ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    tokenResponses = tokens.stream().map(
        token -> {
          TokenFilterResponse tokenFilterResponse = new TokenFilterResponse();
          tokenFilterResponse.setPolicy(token.getPolicy());
          tokenFilterResponse.setName(token.getName());
          tokenFilterResponse.setFingerprint(token.getFingerprint());
          tokenFilterResponse.setDisplayName(token.getNameView());
          tokenFilterResponse.setMetadata(assetMetadataMapper.fromAssetMetadata(
              assetMetadataMap.get(token.getPolicy() + token.getName())));
          return tokenFilterResponse;
        }).toList();
    return tokenResponses;
  }

  /**
   * Explain native script
   * @param nativeScript native script
   * @param nativeScriptResponse native script response
   */
  private void setNativeScriptInfo(NativeScript nativeScript,
                                   NativeScriptResponse nativeScriptResponse) {
    if (nativeScript.getClass().equals(ScriptPubkey.class)) {
      ScriptPubkey scriptPubkey = (ScriptPubkey) nativeScript;
      nativeScriptResponse.getKeyHashes().add(scriptPubkey.getKeyHash());
    } else if (nativeScript.getClass().equals(ScriptAll.class)) {
      ScriptAll scriptAll = (ScriptAll) nativeScript;
      nativeScriptResponse.setConditionType(scriptAll.getType());
      for (NativeScript script : scriptAll.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse);
      }
    } else if (nativeScript.getClass().equals(ScriptAny.class)) {
      ScriptAny scriptAny = (ScriptAny) nativeScript;
      nativeScriptResponse.setConditionType(scriptAny.getType());
      for (NativeScript script : scriptAny.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse);
      }
    } else if (nativeScript.getClass().equals(ScriptAtLeast.class)) {
      ScriptAtLeast scriptAtLeast = (ScriptAtLeast) nativeScript;
      nativeScriptResponse.setConditionType(scriptAtLeast.getType());
      nativeScriptResponse.setRequired(scriptAtLeast.getRequired());
      for (NativeScript script : scriptAtLeast.getScripts()) {
        setNativeScriptInfo(script, nativeScriptResponse);
      }
    } else if (nativeScript.getClass().equals(RequireTimeAfter.class)) {
      RequireTimeAfter requireTimeAfter = (RequireTimeAfter) nativeScript;
      LocalDateTime after
          = slotToTime(requireTimeAfter.getSlot().longValue(), firstBlock, firstShellyBlock);
      nativeScriptResponse.setAfter(after);
    } else if (nativeScript.getClass().equals(RequireTimeBefore.class)) {
      RequireTimeBefore requireTimeBefore = (RequireTimeBefore) nativeScript;
      LocalDateTime before
          = slotToTime(requireTimeBefore.getSlot().longValue(), firstBlock, firstShellyBlock);
      nativeScriptResponse.setBefore(before);
    }
  }

  @Override
  public NativeScriptResponse getNativeScriptDetail(String scriptHash) {
    NativeScriptResponse nativeScriptResponse = new NativeScriptResponse();
    Script script = scriptRepository.findByHash(scriptHash).orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
    );
    Set<ScriptType> nativeScriptTypes = Set.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    if (!nativeScriptTypes.contains(script.getType())) {
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElseGet(() -> NativeScriptInfo.builder()
                .numberOfTokens(multiAssetRepository.countMultiAssetByPolicy(scriptHash))
                .numberOfAssetHolders(multiAssetRepository.countAssetHoldersByPolicy(scriptHash))
                .build());
    nativeScriptResponse.setScriptHash(scriptHash);
    List<String> associatedAddressList = stakeAddressRepository.getStakeAssociatedAddress(scriptHash);
    associatedAddressList.addAll(addressRepository.getAssociatedAddress(scriptHash));
    nativeScriptResponse.setAssociatedAddress(associatedAddressList);
    nativeScriptResponse.setNumberOfTokens(nativeScriptInfo.getNumberOfTokens());
    nativeScriptResponse.setNumberOfAssetHolders(nativeScriptInfo.getNumberOfAssetHolders());
    nativeScriptResponse.setKeyHashes(new ArrayList<>());
    nativeScriptResponse.setVerifiedContract(false);
    try {
      String json = script.getJson();
      if (!StringUtils.isEmpty(json)) {
        nativeScriptResponse.setVerifiedContract(true);
        nativeScriptResponse.setScript(json);
        NativeScript nativeScript = NativeScript.deserializeJson(json);
        setNativeScriptInfo(nativeScript, nativeScriptResponse);
        // One time mint is a native script that has a timelock before the current time
        // and has only one mint transaction
        Long countTxMint = maTxMintRepository.countByPolicy(scriptHash);
        if (Long.valueOf(1L).equals(countTxMint)
            && Objects.nonNull(nativeScriptResponse.getBefore())
            && LocalDateTime.now(ZoneOffset.UTC).isAfter(nativeScriptResponse.getBefore())) {
          if (Objects.isNull(nativeScriptResponse.getConditionType())) {
            nativeScriptResponse.setIsOneTimeMint(true);
          } else nativeScriptResponse.setIsOneTimeMint(
              org.cardanofoundation.ledgersync.common.common.nativescript.ScriptType.all
                  .equals(nativeScriptResponse.getConditionType()));
        } else {
          nativeScriptResponse.setIsOneTimeMint(false);
        }
      }
    } catch (JsonProcessingException | CborDeserializationException e) {
      log.warn("Error parsing script json: {}", e.getMessage());
    }
    return nativeScriptResponse;
  }

  /**
   * Convert slot to time
   * @param slot input slot
   * @param firstBlock first block
   * @param firstShellyBlock first shelly block
   * @return time in UTC
   */
  private LocalDateTime slotToTime(Long slot,
                                   Block firstBlock,
                                   Block firstShellyBlock) {
    if (slot > MAX_SLOT) {
      return LocalDateTime.MAX;
    }
    if (Objects.nonNull(firstShellyBlock)) {
      return firstShellyBlock.getTime().toLocalDateTime()
          .plusSeconds(slot - (firstShellyBlock.getSlotNo()))
          .atZone(ZoneOffset.UTC)
          .toLocalDateTime();
    } else {
      return firstBlock.getTime().toLocalDateTime()
          .plusSeconds(slot)
          .atZone(ZoneOffset.UTC)
          .toLocalDateTime();
    }
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
      if (script.getHash().equals(hash) && StringUtils.isEmpty(scriptJson)) {
        script.setJson(scriptJson);
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
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElse(NativeScriptInfo.builder()
                .numberOfTokens(multiAssetRepository.countMultiAssetByPolicy(scriptHash))
                .build());
    List<MultiAsset> multiAssetList = multiAssetRepository.findMultiAssetByPolicy(scriptHash, pageable);
    Page<MultiAsset> multiAssetPage = new PageImpl<>(multiAssetList, pageable, nativeScriptInfo.getNumberOfTokens());
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
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElse(NativeScriptInfo.builder()
                .numberOfAssetHolders(multiAssetRepository.countAssetHoldersByPolicy(scriptHash))
                .build());
    List<AddressTokenProjection> multiAssetList =
        addressTokenBalanceRepository.findAddressAndBalanceByPolicy(scriptHash, pageable);
    Page<AddressTokenProjection> multiAssetPage =
        new PageImpl<>(multiAssetList, pageable, nativeScriptInfo.getNumberOfAssetHolders());
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
  public BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(
      SmartContractFilterRequest filterRequest, Pageable pageable) {
    scriptMapper.setScriptTxPurpose(filterRequest);
    Page<SmartContractInfo> smartContractProjections =
        smartContractInfoRepository
            .findAllByFilterRequest(filterRequest.getScriptVersion(),
                                    filterRequest.getIsScriptReward(),
                                    filterRequest.getIsScriptCert(),
                                    filterRequest.getIsScriptSpend(),
                                    filterRequest.getIsScriptMint(),
                                    filterRequest.getIsScriptAny(),
                                    filterRequest.getIsScriptNone(),
                                    pageable);

    return new BaseFilterResponse<>(
        smartContractProjections.map(scriptMapper::fromSCInfoToSCFilterResponse));
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
                      addressRepository.getAssociatedAddress(scriptHash).stream())
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

    long txCount = smartContractInfoRepository.getTxCountByScriptHash(scriptHash);
    Page<Long> txIds = new PageImpl<>(
        redeemerRepository.findTxIdsInteractWithContract(scriptHash, pageable), pageable, txCount);

    // get smart contract tx map
    Map<Long, SmartContractTxResponse> smartContractTxMap =
        txRepository.getSmartContractTxsByTxIds(txIds.getContent())
            .stream()
            .map(scriptMapper::fromSmartContractTxProjection)
            .collect(Collectors.toMap(SmartContractTxResponse::getTxId, Function.identity()));

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
      smartContractTxResponse.setScriptPurposeTypes(
          scriptPurposeTypeMap.get(smartContractTxResponse.getTxId()));

      smartContractTxResponses.add(smartContractTxResponse);
    });

    return new BaseFilterResponse<>(txIds, smartContractTxResponses);
  }

  @Override
  public Set<String> getContractExecutions(String txHash, String scriptHash) {
    Set<String> contractExecutions = new HashSet<>();
    List<ContractResponse> contractResponseList = txService
        .getTxDetailByHash(txHash)
        .getContracts();
    contractResponseList.forEach(contractResponse -> {
      if(contractResponse.getScriptHash().equals(scriptHash)) {
        if(!CollectionUtils.isEmpty(contractResponse.getExecutionInputs())) {
          contractExecutions.addAll(contractResponse.getExecutionInputs());
        }
        if(!CollectionUtils.isEmpty(contractResponse.getExecutionOutputs())) {
          contractExecutions.addAll(contractResponse.getExecutionOutputs());
        }
      }
    });
    return contractExecutions.stream().sorted(Comparator.reverseOrder()).collect(
        Collectors.toCollection(LinkedHashSet::new));
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
