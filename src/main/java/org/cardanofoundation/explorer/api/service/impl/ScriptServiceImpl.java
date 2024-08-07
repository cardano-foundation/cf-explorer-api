package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.annotation.PostConstruct;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.bloxbean.cardano.client.exception.CborDeserializationException;
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript;
import com.bloxbean.cardano.client.transaction.spec.script.RequireTimeAfter;
import com.bloxbean.cardano.client.transaction.spec.script.RequireTimeBefore;
import com.bloxbean.cardano.client.transaction.spec.script.ScriptAll;
import com.bloxbean.cardano.client.transaction.spec.script.ScriptAny;
import com.bloxbean.cardano.client.transaction.spec.script.ScriptAtLeast;
import com.bloxbean.cardano.client.transaction.spec.script.ScriptPubkey;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.api.repository.explorer.NativeScriptInfoRepository;
import org.cardanofoundation.explorer.api.repository.explorer.SmartContractInfoRepository;
import org.cardanofoundation.explorer.api.repository.explorer.VerifiedScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.LatestTokenBalanceRepository;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.specification.NativeScriptInfoSpecification;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.explorer.NativeScriptInfo;
import org.cardanofoundation.explorer.common.entity.explorer.SmartContractInfo;
import org.cardanofoundation.explorer.common.entity.explorer.VerifiedScript;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class ScriptServiceImpl implements ScriptService {

  private final ScriptRepository scriptRepository;
  private final NativeScriptInfoRepository nativeScriptInfoRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RedeemerRepository redeemerRepository;
  private final TxRepository txRepository;
  private final AddressRepository addressRepository;
  private final LatestTokenBalanceRepository latestTokenBalanceRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final BlockRepository blockRepository;
  private final VerifiedScriptRepository verifiedScriptRepository;
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
    firstShellyBlock = blockRepository.findFirstShellyBlock().orElse(null);
    firstBlock =
        blockRepository
            .findFirstBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
  }

  @Override
  public BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(
      NativeScriptFilterRequest filterRequest, Pageable pageable) {
    Block currrentBlock =
        blockRepository
            .findLatestBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    Page<NativeScriptInfo> nativeScriptPage =
        nativeScriptInfoRepository.findAll(
            NativeScriptInfoSpecification.filter(currrentBlock.getSlotNo(), filterRequest),
            pageable);
    List<TokenProjection> tokenProjectionList =
        multiAssetRepository.findTopMultiAssetByScriptHashIn(
            nativeScriptPage.stream().map(NativeScriptInfo::getScriptHash).toList());
    List<TokenFilterResponse> tokenResponses =
        tokenProjectionList.stream()
            .map(assetMetadataMapper::fromTokenProjectionToFilterResponse)
            .toList();
    Map<String, List<TokenFilterResponse>> tokenResponseMap =
        tokenResponses.stream().collect(Collectors.groupingBy(TokenFilterResponse::getPolicy));
    Page<NativeScriptFilterResponse> nativeScriptPageResponse =
        nativeScriptPage.map(
            item -> {
              NativeScriptFilterResponse nativeScriptResponse = new NativeScriptFilterResponse();
              nativeScriptResponse.setScriptHash(item.getScriptHash());
              nativeScriptResponse.setNumberOfTokens(item.getNumberOfTokens());
              nativeScriptResponse.setNumberOfAssetHolders(item.getNumberOfAssetHolders());
              if (Objects.nonNull(item.getAfterSlot())) {
                nativeScriptResponse.setAfter(
                    slotToTime(item.getAfterSlot(), firstBlock, firstShellyBlock));
              }
              if (Objects.nonNull(item.getBeforeSlot())) {
                nativeScriptResponse.setBefore(
                    slotToTime(item.getBeforeSlot(), firstBlock, firstShellyBlock));
              }
              if (Objects.nonNull(item.getNumberSig())) {
                nativeScriptResponse.setIsMultiSig(
                    Long.valueOf(1L).compareTo(item.getNumberSig()) < 0);
              }
              nativeScriptResponse.setTokens(tokenResponseMap.get(item.getScriptHash()));
              nativeScriptResponse.setIsOpen(setStatus(item, currrentBlock.getSlotNo()));
              return nativeScriptResponse;
            });
    return new BaseFilterResponse<>(nativeScriptPageResponse);
  }
  /**
   * Explain native script
   *
   * @param nativeScript native script
   * @param nativeScriptResponse native script response
   */
  private void setNativeScriptInfo(
      NativeScript nativeScript, NativeScriptResponse nativeScriptResponse) {
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
      LocalDateTime after = slotToTime(requireTimeAfter.getSlot(), firstBlock, firstShellyBlock);
      nativeScriptResponse.setAfter(after);
    } else if (nativeScript.getClass().equals(RequireTimeBefore.class)) {
      RequireTimeBefore requireTimeBefore = (RequireTimeBefore) nativeScript;
      LocalDateTime before = slotToTime(requireTimeBefore.getSlot(), firstBlock, firstShellyBlock);
      nativeScriptResponse.setBefore(before);
    }
  }

  private boolean setStatus(NativeScriptInfo nativeScriptInfo, Long currentSlot) {
    boolean isOpen = false;
    boolean isNullBefore = Objects.isNull(nativeScriptInfo.getBeforeSlot());
    boolean isNullAfter = Objects.isNull(nativeScriptInfo.getAfterSlot());
    if (!isNullBefore && !isNullAfter) {
      isOpen =
          Long.compare(nativeScriptInfo.getAfterSlot(), currentSlot) == -1
              && Long.compare(nativeScriptInfo.getBeforeSlot(), currentSlot) == 1;
    } else if (isNullAfter && !isNullBefore) {
      isOpen = Long.compare(nativeScriptInfo.getBeforeSlot(), currentSlot) == 1;
    } else if (isNullBefore && !isNullAfter) {
      isOpen = Long.compare(nativeScriptInfo.getAfterSlot(), currentSlot) == -1;
    } else if (isNullAfter && isNullBefore) {
      isOpen = true;
    }
    return isOpen;
  }

  @Override
  public NativeScriptResponse getNativeScriptDetail(String scriptHash) {
    Block currrentBlock =
        blockRepository
            .findLatestBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    NativeScriptResponse nativeScriptResponse = new NativeScriptResponse();
    Script script =
        scriptRepository
            .findByHash(scriptHash)
            .orElseThrow(() -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND));
    Set<ScriptType> nativeScriptTypes = Set.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    if (!nativeScriptTypes.contains(script.getType())) {
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElseGet(
                () ->
                    NativeScriptInfo.builder()
                        .numberOfTokens(multiAssetRepository.countMultiAssetByPolicy(scriptHash))
                        .numberOfAssetHolders(
                            latestTokenBalanceRepository.countAssetHoldersByPolicy(scriptHash))
                        .build());
    nativeScriptResponse.setScriptHash(scriptHash);
    List<String> associatedAddressList =
        stakeAddressRepository.getStakeAssociatedAddress(scriptHash);
    associatedAddressList.addAll(addressRepository.getAssociatedAddress(scriptHash));
    nativeScriptResponse.setAssociatedAddress(associatedAddressList);
    nativeScriptResponse.setNumberOfTokens(nativeScriptInfo.getNumberOfTokens());
    nativeScriptResponse.setNumberOfAssetHolders(nativeScriptInfo.getNumberOfAssetHolders());
    nativeScriptResponse.setKeyHashes(new ArrayList<>());
    nativeScriptResponse.setVerifiedContract(false);
    nativeScriptResponse.setIsOpen(setStatus(nativeScriptInfo, currrentBlock.getSlotNo()));

    String json = script.getJson();
    if (StringUtils.isEmpty(json)) {
      Optional<VerifiedScript> verifiedScript = verifiedScriptRepository.findByHash(scriptHash);
      if (verifiedScript.isPresent() && StringUtils.isEmpty(json)) {
        json = verifiedScript.get().getJson();
      }
    }

    try {
      if (!StringUtils.isEmpty(json)) {
        nativeScriptResponse.setVerifiedContract(true);
        nativeScriptResponse.setScript(json);
        NativeScript nativeScript = NativeScript.deserializeJson(json);
        setNativeScriptInfo(nativeScript, nativeScriptResponse);
        // One time mint is a native script that has a timelock before the current time
        // and has only one mint transaction
        if (Objects.nonNull(nativeScriptResponse.getBefore())
            && LocalDateTime.now(ZoneOffset.UTC).isAfter(nativeScriptResponse.getBefore())
            && isOneTimeMint(scriptHash)) {
          nativeScriptResponse.setIsOneTimeMint(
              Objects.isNull(nativeScriptResponse.getConditionType())
                  || com.bloxbean.cardano.client.transaction.spec.script.ScriptType.all.equals(
                      nativeScriptResponse.getConditionType()));
        } else {
          nativeScriptResponse.setIsOneTimeMint(false);
        }
      }
    } catch (JsonProcessingException | CborDeserializationException e) {
      log.warn("Error parsing script json: {}", e.getMessage());
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }
    return nativeScriptResponse;
  }

  /**
   * Convert slot to time
   *
   * @param slot input slot
   * @param firstBlock first block
   * @param firstShellyBlock first shelly block
   * @return time in UTC
   */
  private LocalDateTime slotToTime(Long slot, Block firstBlock, Block firstShellyBlock) {
    if (slot > MAX_SLOT) {
      return LocalDateTime.MAX;
    }
    if (Objects.nonNull(firstShellyBlock)) {
      return firstShellyBlock
          .getTime()
          .toLocalDateTime()
          .plusSeconds(slot - (firstShellyBlock.getSlotNo()))
          .atZone(ZoneOffset.UTC)
          .toLocalDateTime();
    } else {
      return firstBlock
          .getTime()
          .toLocalDateTime()
          .plusSeconds(slot)
          .atZone(ZoneOffset.UTC)
          .toLocalDateTime();
    }
  }

  @Override
  @Transactional
  public String verifyNativeScript(String scriptHash, String scriptJson) {
    try {
      Script script =
          scriptRepository
              .findByHash(scriptHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND));
      Set<ScriptType> nativeScriptTypes = Set.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
      if (!nativeScriptTypes.contains(script.getType())) {
        throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
      }
      if (Boolean.TRUE.equals(verifiedScriptRepository.existsVerifiedScriptByHash(scriptHash))) {
        throw new BusinessException(BusinessCode.SCRIPT_ALREADY_VERIFIED);
      }
      String hash = Hex.encodeHexString(NativeScript.deserializeJson(scriptJson).getScriptHash());
      if (script.getHash().equals(hash) && StringUtils.isEmpty(script.getJson())) {
        VerifiedScript verifiedScript =
            VerifiedScript.builder().hash(scriptHash).json(scriptJson).build();
        verifiedScriptRepository.save(verifiedScript);
        return scriptJson;
      } else {
        throw new BusinessException(BusinessCode.VERIFY_SCRIPT_FAILED);
      }
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.VERIFY_SCRIPT_FAILED);
    }
  }

  @Override
  public BaseFilterResponse<TokenFilterResponse> getNativeScriptTokens(
      String scriptHash, Pageable pageable) {
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElseGet(
                () ->
                    NativeScriptInfo.builder()
                        .numberOfTokens(multiAssetRepository.countMultiAssetByPolicy(scriptHash))
                        .build());
    List<TokenFilterResponse> tokenFilterResponses =
        multiAssetRepository.findTokenInfoByScriptHash(scriptHash, pageable).stream()
            .map(assetMetadataMapper::fromTokenProjectionToFilterResponse)
            .toList();
    Page<TokenFilterResponse> tokenPage =
        new PageImpl<>(tokenFilterResponses, pageable, nativeScriptInfo.getNumberOfTokens());
    return new BaseFilterResponse<>(tokenPage);
  }

  @Override
  public BaseFilterResponse<TokenAddressResponse> getNativeScriptHolders(
      String scriptHash, Pageable pageable) {
    NativeScriptInfo nativeScriptInfo =
        nativeScriptInfoRepository
            .findByScriptHash(scriptHash)
            .orElseGet(
                () ->
                    NativeScriptInfo.builder()
                        .numberOfAssetHolders(
                            latestTokenBalanceRepository.countAssetHoldersByPolicy(scriptHash))
                        .build());
    List<AddressTokenProjection> addressTokenMap =
        latestTokenBalanceRepository.findAddressAndBalanceByPolicy(scriptHash, pageable);

    Map<String, MultiAsset> multiAssetMap =
        multiAssetRepository
            .findAllByUnitIn(addressTokenMap.stream().map(AddressTokenProjection::getUnit).toList())
            .stream()
            .collect(Collectors.toMap(MultiAsset::getUnit, Function.identity()));

    Page<AddressTokenProjection> addressTokenPage =
        new PageImpl<>(addressTokenMap, pageable, nativeScriptInfo.getNumberOfAssetHolders());

    Page<TokenAddressResponse> tokenAddressResponses =
        addressTokenPage.map(
            addressTokenProjection -> {
              MultiAsset multiAsset = multiAssetMap.get(addressTokenProjection.getUnit());
              return tokenMapper.fromAddressTokenProjectionAndMultiAsset(
                  addressTokenProjection, multiAsset);
            });

    return new BaseFilterResponse<>(tokenAddressResponses);
  }

  @Override
  public BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(
      SmartContractFilterRequest filterRequest, Pageable pageable) {
    scriptMapper.setScriptTxPurpose(filterRequest);
    Page<SmartContractInfo> smartContractProjections =
        smartContractInfoRepository.findAllByFilterRequest(
            filterRequest.getScriptVersion(),
            filterRequest.getIsScriptReward(),
            filterRequest.getIsScriptCert(),
            filterRequest.getIsScriptSpend(),
            filterRequest.getIsScriptMint(),
            filterRequest.getIsScriptVote(),
            filterRequest.getIsScriptPropose(),
            filterRequest.getIsScriptAny(),
            filterRequest.getIsScriptNone(),
            pageable);

    return new BaseFilterResponse<>(
        smartContractProjections.map(scriptMapper::fromSCInfoToSCFilterResponse));
  }

  @Override
  public SmartContractDetailResponse getSmartContractDetail(String scriptHash) {
    Script script =
        scriptRepository
            .findByHash(scriptHash)
            .orElseThrow(() -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND));

    if (!script.getType().equals(ScriptType.PLUTUSV1)
        && !script.getType().equals(ScriptType.PLUTUSV2)
        && !script.getType().equals(ScriptType.PLUTUSV3)) {
      throw new BusinessException(BusinessCode.SCRIPT_NOT_FOUND);
    }

    List<String> associatedAddresses =
        Stream.concat(
                stakeAddressRepository.getAssociatedAddress(scriptHash).stream(),
                addressRepository.getAssociatedAddress(scriptHash).stream())
            .toList();

    return SmartContractDetailResponse.builder()
        .scriptHash(script.getHash())
        .scriptType(script.getType())
        .associatedAddresses(associatedAddresses)
        .build();
  }

  @Override
  public BaseFilterResponse<SmartContractTxResponse> getSmartContractTxs(
      String scriptHash, Pageable pageable) {

    long txCount = smartContractInfoRepository.getTxCountByScriptHash(scriptHash).orElse(0L);
    Page<Long> txIds =
        new PageImpl<>(
            redeemerRepository.findTxIdsInteractWithContract(scriptHash, pageable),
            pageable,
            txCount);

    // get smart contract tx map
    Map<Long, SmartContractTxResponse> smartContractTxMap =
        txRepository.getSmartContractTxsByTxIds(txIds.getContent()).stream()
            .map(scriptMapper::fromSmartContractTxProjection)
            .collect(Collectors.toMap(SmartContractTxResponse::getTxId, Function.identity()));

    // get script purpose type map
    Map<Long, List<ScriptPurposeType>> scriptPurposeTypeMap =
        txRepository.getSmartContractTxsPurpose(txIds.getContent(), scriptHash).stream()
            .collect(Collectors.groupingBy(SmartContractTxProjection::getTxId))
            .entrySet()
            .stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    e ->
                        e.getValue().stream()
                            .map(SmartContractTxProjection::getScriptPurposeType)
                            .toList()));

    List<SmartContractTxResponse> smartContractTxResponses = new ArrayList<>();
    txIds.stream()
        .forEach(
            txId -> {
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
    List<ContractResponse> contractResponseList =
        txService.getTxDetailByHash(txHash).getContracts();
    contractResponseList.forEach(
        contractResponse -> {
          if (scriptHash.equals(contractResponse.getScriptHash())) {
            if (!CollectionUtils.isEmpty(contractResponse.getExecutionInputs())) {
              contractExecutions.addAll(contractResponse.getExecutionInputs());
            }
            if (!CollectionUtils.isEmpty(contractResponse.getExecutionOutputs())) {
              contractExecutions.addAll(contractResponse.getExecutionOutputs());
            }
          }
        });
    return contractExecutions.stream()
        .sorted(Comparator.reverseOrder())
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }

  @Override
  public ScriptSearchResponse searchScript(String scriptHash) {
    Script script =
        scriptRepository
            .findByHash(scriptHash)
            .orElseThrow(() -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND));
    boolean isSmartContract =
        ScriptType.PLUTUSV1.equals(script.getType())
            || ScriptType.PLUTUSV2.equals(script.getType())
            || ScriptType.PLUTUSV3.equals(script.getType());
    ScriptSearchResponse scriptSearchResponse =
        ScriptSearchResponse.builder().scriptHash(script.getHash()).build();

    if (isSmartContract) {
      scriptSearchResponse.setSmartContract(true);
    } else {
      scriptSearchResponse.setNativeScript(true);
    }
    return scriptSearchResponse;
  }

  /**
   * Determines if a given script hash corresponds to a one-time mint. This method checks if there
   * is only one mint transaction associated with the multi-assets identified by the given script
   * hash. It iterates through the multi-assets, checking each for multiple mint transactions. If
   * any multi-asset has more than one mint transaction, the method returns false, indicating that
   * the script hash is not a one-time mint.
   *
   * @param scriptHash The script hash
   * @return true if the script hash type of one-time mint, false otherwise.
   */
  private boolean isOneTimeMint(String scriptHash) {
    Pageable pageable = PageRequest.of(0, 100, Sort.by("id").ascending());
    Slice<MultiAsset> multiAssetSlice = multiAssetRepository.getSliceByPolicy(scriptHash, pageable);
    List<Long> idents =
        multiAssetSlice.getContent().stream().map(MultiAsset::getId).collect(Collectors.toList());
    Long firstTxMintId = maTxMintRepository.findFirstTxMintByMultiAssetId(idents.get(0));

    if (Boolean.TRUE.equals(
        maTxMintRepository.existsMoreOneMintTx(multiAssetSlice.getContent(), firstTxMintId))) {
      return false;
    }

    boolean isOneTimeMint = true;
    while (multiAssetSlice.hasNext()) {
      multiAssetSlice =
          multiAssetRepository.getSliceByPolicy(scriptHash, multiAssetSlice.nextPageable());
      idents =
          multiAssetSlice.getContent().stream().map(MultiAsset::getId).collect(Collectors.toList());
      firstTxMintId = maTxMintRepository.findFirstTxMintByMultiAssetId(idents.get(0));
      if (Boolean.TRUE.equals(
          maTxMintRepository.existsMoreOneMintTx(multiAssetSlice.getContent(), firstTxMintId))) {
        isOneTimeMint = false;
        break;
      }
    }

    return isOneTimeMint;
  }
}
