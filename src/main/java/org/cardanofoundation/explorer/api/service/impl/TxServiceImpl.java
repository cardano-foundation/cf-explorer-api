package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.bloxbean.cardano.client.crypto.Blake2bUtil;
import com.bloxbean.cardano.client.util.AssetUtil;
import com.bloxbean.cardano.client.util.HexUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.*;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRelayResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRelayProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.tx.*;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.FailedTxOutRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolRelayRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ReferenceTxInRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ReserveRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TokenTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TreasuryRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxBootstrapWitnessesRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxChartRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxWitnessesRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.UnconsumeTxInRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.BolnisiMetadataService;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.util.*;
import org.cardanofoundation.explorer.common.entity.ledgersync.*;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;
  private final TxOutMapper txOutMapper;
  private final TokenMapper tokenMapper;
  private final RedeemerRepository redeemerRepository;
  private final EpochRepository epochRepository;
  private final UnconsumeTxInRepository unconsumeTxInRepository;
  private final FailedTxOutRepository failedTxOutRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final AddressRepository addressRepository;
  private final WithdrawalMapper withdrawalMapper;
  private final DelegationRepository delegationRepository;
  private final DelegationMapper delegationMapper;
  private final MaTxMintRepository maTxMintRepository;
  private final MaTxMintMapper maTxMintMapper;
  private final AddressTxAmountRepository addressTxAmountRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final AddressTxCountRepository addressTxCountRepository;
  private final StakeAddressTxCountRepository stakeAddressTxCountRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AssetMetadataMapper assetMetadataMapper;
  private final StakeRegistrationRepository stakeRegistrationRepository;
  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;
  private final PoolUpdateRepository poolUpdateRepository;
  private final PoolRelayRepository poolRelayRepository;
  private final PoolRetireRepository poolRetireRepository;
  private final ParamProposalRepository paramProposalRepository;
  private final EpochParamRepository epochParamRepository;
  private final ProtocolMapper protocolMapper;
  private final TxChartRepository txChartRepository;
  private final TreasuryRepository treasuryRepository;
  private final ReserveRepository reserveRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final TxContractMapper txContractMapper;
  private final TxMetadataRepository txMetadataRepository;
  private final TxWitnessesRepository txWitnessesRepository;
  private final TxBootstrapWitnessesRepository txBootstrapWitnessesRepository;
  private final ProtocolParamService protocolParamService;
  private final ReferenceTxInRepository referenceTxInRepository;
  private final TxReferenceInputMapper txReferenceInputMapper;
  private final BolnisiMetadataService bolnisiMetadataService;

  private final RedisTemplate<String, TxGraph> redisTemplate;
  private static final int SUMMARY_SIZE = 4;
  public static final long HOURS_IN_DAY = 24;
  public static final long DAY_IN_WEEK = 7;
  public static final long DAY_IN_TWO_WEEK = DAY_IN_WEEK * 2;
  public static final long DAYS_IN_MONTH = 32;

  private static final String UNIT_LOVELACE = "lovelace";
  private static final String TRANSACTION_GRAPH_MONTH_KEY = "TRANSACTION_GRAPH_MONTH";
  private final TokenTxCountRepository tokenTxCountRepository;

  @Value("${application.network}")
  private String network;

  @Override
  public List<TxSummary> findLatestTxSummary() {
    List<Long> txIds =
        txRepository.findLatestTxId(
            PageRequest.of(
                BigInteger.ZERO.intValue(), SUMMARY_SIZE, Sort.by(BaseEntity_.ID).descending()));

    if (txIds.isEmpty()) {
      return Collections.emptyList();
    }

    List<TxSummary> summaries = new ArrayList<>();
    List<TxIOProjection> txs = txRepository.findLatestTxIO(txIds);

    txs.forEach(
        tx -> {
          Optional<TxSummary> searchedSummary =
              summaries.stream()
                  .filter(summary -> summary.getHash().equals(tx.getHash()))
                  .findFirst();

          if (searchedSummary.isEmpty()) {

            final var from = new ArrayList<String>();
            from.add(tx.getFromAddress());

            final var to = new ArrayList<String>();
            to.add(tx.getToAddress());

            final TxSummary summary =
                TxSummary.builder()
                    .blockNo(tx.getBlockNo())
                    .hash(tx.getHash())
                    .amount(tx.getAmount().doubleValue())
                    .fromAddress(from)
                    .toAddress(to)
                    .epochNo(tx.getEpochNo())
                    .epochSlotNo(tx.getEpochSlotNo())
                    .slot(tx.getSlot())
                    .time(tx.getTime())
                    .status(
                        Boolean.TRUE.equals(tx.getValidContract())
                            ? TxStatus.SUCCESS
                            : TxStatus.FAILED)
                    .build();
            summaries.add(summary);
            return;
          }

          final TxSummary summary = searchedSummary.get();
          if (!summary.getFromAddress().contains(tx.getFromAddress())) {
            summary.getFromAddress().add(tx.getFromAddress());
          }

          if (!summary.getToAddress().contains(tx.getToAddress())) {
            summary.getToAddress().add(tx.getToAddress());
          }
        });

    return summaries;
  }

  @Override
  public List<TxGraph> getTransactionChartByRange(TxChartRange range) {
    if (range.equals(TxChartRange.ONE_DAY)) {
      return getTransactionChartInOneDay();
    }
    long day = DAY_IN_WEEK;

    if (range.equals(TxChartRange.TWO_WEEK)) {
      day = DAY_IN_TWO_WEEK;
    } else if (range.equals(TxChartRange.ONE_MONTH)) {
      day = DAYS_IN_MONTH;
    }
    return getTransactionChartInRange(day);
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getAll(Pageable pageable) {
    Page<Tx> txPage = txRepository.findAllTx(pageable);
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByBlock(
      String blockId, Pageable pageable) {
    Page<Tx> txPage;
    try {
      Long blockNo = Long.parseLong(blockId);
      txPage = txRepository.findByBlockNo(blockNo, pageable);
    } catch (NumberFormatException e) {
      txPage = txRepository.findByBlockHash(blockId, pageable);
    }
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByAddress(
      String address, Pageable pageable) {
    addressRepository
        .findFirstByAddress(address)
        .orElseThrow(() -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND));

    AddressTxCount addressTxCount =
        addressTxCountRepository.findById(address).orElse(new AddressTxCount(address, 0L));

    List<TxProjection> txProjections =
        addressTxAmountRepository.findAllTxByAddress(address, pageable);
    List<String> txHashes = txProjections.stream().map(TxProjection::getTxHash).toList();
    List<AddressTxAmount> addressTxAmounts =
        addressTxAmountRepository.findAllByAddressAndTxHashIn(address, txHashes);

    Page<TxFilterResponse> txFilterResponsePage =
        new PageImpl<>(
            mapTxDataFromAddressTxAmount(txProjections, addressTxAmounts),
            pageable,
            addressTxCount.getTxCount());

    return new BaseFilterResponse<>(txFilterResponsePage);
  }

  private List<TxFilterResponse> mapTxDataFromAddressTxAmount(
      List<TxProjection> txProjections, List<AddressTxAmount> addressTxAmounts) {

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    List<String> txHashes = txProjections.stream().map(TxProjection::getTxHash).toList();

    Map<String, Tx> txMap =
        txRepository.findAllByHashIn(txHashes).stream()
            .collect(Collectors.toMap(Tx::getHash, Function.identity()));

    List<MultiAsset> multiAssets =
        multiAssetRepository.findAllByUnitIn(
            addressTxAmounts.stream().map(AddressTxAmount::getUnit).collect(Collectors.toSet()));

    Map<Long, Block> blockMap =
        blockRepository
            .findAllByIdIn(txMap.values().stream().map(Tx::getBlockId).collect(Collectors.toList()))
            .stream()
            .collect(Collectors.toMap(Block::getId, Function.identity()));

    Map<String, MultiAsset> unitMultiAssetMap =
        multiAssets.stream().collect(Collectors.toMap(MultiAsset::getUnit, Function.identity()));

    Map<String, AssetMetadata> fingerprintAssetMetadataMap =
        assetMetadataRepository
            .findByFingerprintIn(
                multiAssets.stream().map(MultiAsset::getFingerprint).collect(Collectors.toSet()))
            .stream()
            .collect(Collectors.toMap(AssetMetadata::getFingerprint, Function.identity()));

    Map<String, Map<String, List<AddressTxAmount>>> addressTxAmountMap =
        addressTxAmounts.stream()
            .collect(
                Collectors.groupingBy(
                    AddressTxAmount::getTxHash, Collectors.groupingBy(AddressTxAmount::getUnit)));

    txProjections.forEach(
        txProjection -> {
          TxFilterResponse txFilterResponse = new TxFilterResponse();
          Tx tx = txMap.get(txProjection.getTxHash());
          String txHash = tx.getHash();
          Block block = blockMap.get(tx.getBlockId());
          BigInteger balance =
              addressTxAmountMap.get(txHash).get(UNIT_LOVELACE).stream()
                  .reduce(BigInteger.ZERO, (a, b) -> a.add(b.getQuantity()), BigInteger::add);

          List<AddressTxAmount> tokenQuantityChange =
              addressTxAmountMap.get(txHash).entrySet().stream()
                  .filter(entry -> !entry.getKey().equals(UNIT_LOVELACE))
                  .flatMap(entry -> entry.getValue().stream())
                  .collect(Collectors.toList());

          txFilterResponse.setHash(txHash);
          txFilterResponse.setTime(block.getTime().toLocalDateTime());
          txFilterResponse.setBlockNo(block.getBlockNo());
          txFilterResponse.setEpochNo(block.getEpochNo());
          txFilterResponse.setSlot(block.getSlotNo().intValue());
          txFilterResponse.setEpochSlotNo(block.getEpochSlotNo());
          txFilterResponse.setFee(tx.getFee());
          txFilterResponse.setBalance(balance);
          txFilterResponse.setTotalOutput(tx.getOutSum());
          txFilterResponse.setTokens(
              getTokenQuantityChangeResponse(
                  tokenQuantityChange, unitMultiAssetMap, fingerprintAssetMetadataMap));

          txFilterResponses.add(txFilterResponse);
        });

    return txFilterResponses;
  }

  private List<TokenAddressResponse> getTokenQuantityChangeResponse(
      List<AddressTxAmount> tokenQuantityChange,
      Map<String, MultiAsset> unitMultiAssetMap,
      Map<String, AssetMetadata> fingerprintAssetMetadataMap) {

    List<TokenAddressResponse> tokenAddressResponses = new ArrayList<>();
    tokenQuantityChange.forEach(
        addressTxAmount -> {
          if (BigInteger.ZERO.equals(addressTxAmount.getQuantity())) {
            return;
          }
          MultiAsset multiAsset = unitMultiAssetMap.get(addressTxAmount.getUnit());
          TokenAddressResponse tokenAddressResponse = new TokenAddressResponse();
          if (!Objects.isNull(multiAsset)) {
            tokenAddressResponse =
                tokenMapper.fromMultiAssetAndAddressToken(multiAsset, addressTxAmount);
            tokenAddressResponse.setMetadata(
                assetMetadataMapper.fromAssetMetadata(
                    fingerprintAssetMetadataMap.get(multiAsset.getFingerprint())));
          }
          tokenAddressResponses.add(tokenAddressResponse);
        });

    return tokenAddressResponses;
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByToken(
      String tokenId, Pageable pageable) {

    MultiAsset multiAsset =
        multiAssetRepository
            .findByFingerprint(tokenId)
            .orElseThrow(() -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));

    TokenTxCount tokenTxCount =
        tokenTxCountRepository
            .findById(multiAsset.getId())
            .orElse(new TokenTxCount(multiAsset.getId(), 0L));

    List<TxProjection> txsProjection =
        addressTxAmountRepository.findAllTxByUnit(multiAsset.getUnit(), pageable);
    List<Tx> txs =
        txRepository.findAllByHashIn(
            txsProjection.stream().map(TxProjection::getTxHash).collect(Collectors.toList()));
    Page<Tx> txPage = new PageImpl<>(txs, pageable, tokenTxCount.getTxCount());

    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByStake(
      String stakeKey, Pageable pageable) {

    stakeAddressRepository
        .findByView(stakeKey)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    StakeAddressTxCount addressTxCount =
        stakeAddressTxCountRepository
            .findById(stakeKey)
            .orElse(new StakeAddressTxCount(stakeKey, 0L));

    List<TxProjection> txProjections =
        addressTxAmountRepository.findAllTxByStakeAddress(stakeKey, pageable);
    List<String> txHashes = txProjections.stream().map(TxProjection::getTxHash).toList();
    List<AddressTxAmount> addressTxAmounts =
        addressTxAmountRepository.findAllByStakeAddressAndTxHashIn(stakeKey, txHashes);

    Page<TxFilterResponse> txFilterResponsePage =
        new PageImpl<>(
            mapTxDataFromAddressTxAmount(txProjections, addressTxAmounts),
            pageable,
            addressTxCount.getTxCount());

    return new BaseFilterResponse<>(txFilterResponsePage);
  }

  /**
   * Mapping from tx entity list to tx response dto
   *
   * @param txPage list tx in page
   * @return list tx response
   */
  private List<TxFilterResponse> mapDataFromTxListToResponseList(Page<Tx> txPage) {
    if (CollectionUtils.isEmpty(txPage.getContent())) {
      return new ArrayList<>();
    }
    Set<Long> blockIdList =
        txPage.getContent().stream().map(Tx::getBlockId).collect(Collectors.toSet());
    List<Block> blocks = blockRepository.findAllByIdIn(blockIdList);
    Map<Long, Block> blockMap =
        blocks.stream().collect(Collectors.toMap(Block::getId, Function.identity()));
    // get addresses input
    Set<Long> txIdSet = txPage.getContent().stream().map(Tx::getId).collect(Collectors.toSet());
    List<AddressInputOutputProjection> txInList =
        txOutRepository.findAddressInputListByTxId(txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressInMap =
        txInList.stream().collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    // get addresses output
    List<AddressInputOutputProjection> txOutList =
        txOutRepository.findAddressOutputListByTxId(txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressOutMap =
        txOutList.stream().collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (Tx tx : txPage.getContent()) {
      Long txId = tx.getId();
      if (blockMap.containsKey(tx.getBlockId())) {
        tx.setBlock(blockMap.get(tx.getBlockId()));
      }
      TxFilterResponse txResponse = txMapper.txToTxFilterResponse(tx);
      if (addressOutMap.containsKey(txId)) {
        txResponse.setAddressesOutput(
            addressOutMap.get(tx.getId()).stream()
                .map(AddressInputOutputProjection::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesOutput(new ArrayList<>());
      }
      if (addressInMap.containsKey(txId)) {
        txResponse.setAddressesInput(
            addressInMap.get(tx.getId()).stream()
                .map(AddressInputOutputProjection::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesInput(new ArrayList<>());
      }
      txFilterResponses.add(txResponse);
    }
    return txFilterResponses;
  }

  private Pair<Map<String, AssetMetadata>, Map<Long, MultiAsset>> getMapMetadataAndMapAsset(
      List<Long> multiAssetIdList) {
    List<MultiAsset> multiAssets = multiAssetRepository.findAllByIdIn(multiAssetIdList);
    Set<String> subjects =
        multiAssets.stream().map(ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap =
        assetMetadataList.stream()
            .collect(Collectors.toMap(AssetMetadata::getSubject, Function.identity()));

    Map<Long, MultiAsset> multiAssetMap =
        multiAssets.stream().collect(Collectors.toMap(MultiAsset::getId, Function.identity()));

    return Pair.of(assetMetadataMap, multiAssetMap);
  }

  @Override
  @Transactional(readOnly = true)
  public TxResponse getTxDetailByHash(String hash) {
    Tx tx =
        txRepository
            .findByHash(hash)
            .orElseThrow(() -> new BusinessException(BusinessCode.TRANSACTION_NOT_FOUND));
    Integer currentBlockNo =
        blockRepository
            .findCurrentBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    TxResponse txResponse = txMapper.txToTxResponse(tx);

    if (Objects.nonNull(txResponse.getTx().getEpochNo())) {
      Epoch epoch =
          epochRepository
              .findFirstByNo(txResponse.getTx().getEpochNo())
              .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      txResponse.getTx().setMaxEpochSlot(epoch.getMaxSlot());
    }
    if (Objects.nonNull(txResponse.getTx().getBlockNo())) {
      txResponse.getTx().setConfirmation(currentBlockNo - txResponse.getTx().getBlockNo());
    } else {
      txResponse.getTx().setConfirmation(currentBlockNo);
    }

    // get address input output
    getSummaryAndUTxOs(tx, txResponse);
    getCollaterals(tx, txResponse);
    getWithdrawals(tx, txResponse);
    getDelegations(tx, txResponse);
    getMints(tx, txResponse);
    getStakeCertificates(tx, txResponse);
    getPoolCertificates(tx, txResponse);
    getProtocols(tx, txResponse);
    getInstantaneousRewards(tx, txResponse);
    getMetadata(tx, txResponse);
    getSignersInformation(tx, txResponse);
    /*
     * If the transaction is invalid, the collateral is the input and the output of the transaction.
     * Otherwise, the collateral is the input and the output of the collateral.
     */
    if (Boolean.TRUE.equals(tx.getValidContract())) {
      txResponse.getTx().setStatus(TxStatus.SUCCESS);
    } else {
      txResponse.getTx().setStatus(TxStatus.FAILED);
      CollateralResponse collateralResponse = txResponse.getCollaterals();
      List<TxOutResponse> collateralInputs = collateralResponse.getCollateralInputResponses();
      List<TxOutResponse> collateralOutputs = collateralResponse.getCollateralOutputResponses();
      collateralResponse.setCollateralInputResponses(txResponse.getUTxOs().getInputs());
      collateralResponse.setCollateralOutputResponses(txResponse.getUTxOs().getOutputs());
      BigInteger totalInput =
          collateralResponse.getCollateralInputResponses().stream()
              .reduce(BigInteger.ZERO, (a, b) -> a.add(b.getValue()), BigInteger::add);
      BigInteger totalOutput =
          collateralResponse.getCollateralOutputResponses().stream()
              .reduce(BigInteger.ZERO, (a, b) -> a.add(b.getValue()), BigInteger::add);
      txResponse.getTx().setFee(totalInput.subtract(totalOutput));
      txResponse.setCollaterals(collateralResponse);
      UTxOResponse uTxOResponse = new UTxOResponse();
      uTxOResponse.setInputs(collateralInputs);
      uTxOResponse.setOutputs(collateralOutputs);
      txResponse.setUTxOs(uTxOResponse);
    }
    getContracts(tx, txResponse);

    return txResponse;
  }

  /**
   * Get signers (Delegate Keys) Information for MIR (instantaneous rewards) Transaction
   *
   * @param tx
   * @param txResponse
   */
  private void getSignersInformation(Tx tx, TxResponse txResponse) {
    List<TxWitness> txVkeyWitnesses = txWitnessesRepository.findAllByTx(tx);
    List<TxBootstrapWitnesses> txBootstrapWitnesses =
        txBootstrapWitnessesRepository.findAllByTx(tx);
    if (!CollectionUtils.isEmpty(txVkeyWitnesses)) {
      Map<Integer, String> signersIndexMap = new HashMap<>();
      txVkeyWitnesses.forEach(
          txVkeyWitness ->
              Arrays.stream(txVkeyWitness.getIndexArr())
                  .forEach(index -> signersIndexMap.put(index, txVkeyWitness.getKey())));

      List<TxSignersResponse> txSignersResponses =
          Stream.concat(
                  signersIndexMap.entrySet().stream()
                      .sorted(Map.Entry.comparingByKey())
                      .map(
                          entry -> TxSignersResponse.builder().publicKey(entry.getValue()).build()),
                  txBootstrapWitnesses.stream()
                      .map(
                          txBootstrapWitness ->
                              TxSignersResponse.builder()
                                  .publicKey(txBootstrapWitness.getPublicKey())
                                  .build()))
              .toList();

      if (Objects.nonNull(txResponse.getProtocols())
          || !CollectionUtils.isEmpty(txResponse.getInstantaneousRewards())) {
        Map<String, String> dKeyHash224Map = protocolParamService.getGenesisDelegateKeysMap();
        txSignersResponses.forEach(
            txSignersResponse -> {
              byte[] pkeyHash224 =
                  Blake2bUtil.blake2bHash224(
                      HexUtil.decodeHexString(txSignersResponse.getPublicKey()));
              String pkeyHash224Hex = HexUtil.encodeHexString(pkeyHash224);
              if (dKeyHash224Map.containsKey(pkeyHash224Hex)) {
                txSignersResponse.setDelegateKey(dKeyHash224Map.get(pkeyHash224Hex));
              }
            });
      }

      txResponse.setSignersInformation(txSignersResponses);
    }
  }

  /**
   * Get transaction metadata
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getMetadata(Tx tx, TxResponse txResponse) {
    List<TxMetadataResponse> txMetadataList =
        txMetadataRepository.findAllByTxOrderByKeyAsc(tx).stream()
            .map(
                txMetadata ->
                    TxMetadataResponse.builder()
                        .label(txMetadata.getKey())
                        .value(txMetadata.getJson())
                        .metadataCIP25(
                            txMetadata.getKey().equals(BigInteger.valueOf(721))
                                ? MetadataCIP25Utils.standard(txMetadata.getJson())
                                : null)
                        .metadataCIP60(
                            txMetadata.getKey().equals(BigInteger.valueOf(721))
                                ? MetadataCIP60Utils.standard(txMetadata.getJson())
                                : null)
                        .metadataCIP20(
                            txMetadata.getKey().equals(BigInteger.valueOf(674))
                                ? MetadataCIP20Utils.standard(txMetadata.getJson())
                                : null)
                        .metadataCIP83(
                            txMetadata.getKey().equals(BigInteger.valueOf(674))
                                ? MetadataCIP83Utils.standard(txMetadata.getJson())
                                : null)
                        .metadataBolnisi(
                            txMetadata.getKey().equals(BigInteger.valueOf(1904))
                                ? bolnisiMetadataService.getBolnisiMetadata(txMetadata.getJson())
                                : null)
                        .build())
            .toList();
    if (!CollectionUtils.isEmpty(txMetadataList)) {
      txResponse.setMetadata(txMetadataList);
    }
    if (Objects.nonNull(tx.getTxMetadataHash())) {
      txResponse.setMetadataHash(tx.getTxMetadataHash().getHash());
    }
  }

  /**
   * Get transaction reserve and treasury
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getInstantaneousRewards(Tx tx, TxResponse txResponse) {
    var instantaneousRewards = reserveRepository.findByTx(tx);
    instantaneousRewards.addAll(treasuryRepository.findByTx(tx));
    if (!CollectionUtils.isEmpty(instantaneousRewards)) {
      txResponse.setInstantaneousRewards(instantaneousRewards);
    }
  }

  /**
   * Get transaction mints info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getMints(Tx tx, TxResponse txResponse) {
    List<MintProjection> maTxMints = maTxMintRepository.findByTxId(tx.getId());
    if (!CollectionUtils.isEmpty(maTxMints)) {
      txResponse.setMints(
          maTxMints.stream()
              .map(maTxMintMapper::fromMintProjectionToTxMintingResponse)
              .collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction delegations info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getDelegations(Tx tx, TxResponse txResponse) {
    List<Delegation> delegations = delegationRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(delegations)) {
      txResponse.setDelegations(
          delegations.stream().map(delegationMapper::fromDelegation).collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction withdrawals info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getWithdrawals(Tx tx, TxResponse txResponse) {
    List<Withdrawal> withdrawals = withdrawalRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(withdrawals)) {
      List<String> addressToList =
          txResponse.getUTxOs().getOutputs().stream()
              .map(TxOutResponse::getAddress)
              .collect(Collectors.toList());
      List<WithdrawalResponse> withdrawalResponses =
          withdrawals.stream()
              .map(
                  withdrawal -> {
                    WithdrawalResponse withdrawalResponse =
                        withdrawalMapper.fromWithdrawal(withdrawal);
                    withdrawalResponse.setAddressTo(addressToList);
                    return withdrawalResponse;
                  })
              .collect(Collectors.toList());

      txResponse.setWithdrawals(withdrawalResponses);
    }
  }

  /**
   * Get transaction collaterals info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getCollaterals(Tx tx, TxResponse txResponse) {
    CollateralResponse collateralResponse = new CollateralResponse();
    List<AddressInputOutputProjection> collateralInputs =
        unconsumeTxInRepository.findTxCollateralInput(tx);
    List<AddressInputOutputProjection> collateralOutputs =
        failedTxOutRepository.findFailedTxOutByTx(tx);
    if (!CollectionUtils.isEmpty(collateralInputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap =
          collateralInputs.stream()
              .collect(Collectors.groupingBy(txOutMapper::fromAddressInputOutput));
      List<TxOutResponse> collateralInputResponse = mappingProjectionToAddress(addressInputMap);
      collateralResponse.setCollateralInputResponses(collateralInputResponse);
    }
    if (!CollectionUtils.isEmpty(collateralOutputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap =
          collateralOutputs.stream()
              .collect(Collectors.groupingBy(txOutMapper::fromAddressInputOutput));
      List<TxOutResponse> collateralOutputResponses = mappingProjectionToAddress(addressOutputMap);
      collateralResponse.setCollateralOutputResponses(collateralOutputResponses);
    }
    if (CollectionUtils.isEmpty(collateralInputs) && CollectionUtils.isEmpty(collateralOutputs)) {
      txResponse.setCollaterals(null);
    } else {
      txResponse.setCollaterals(collateralResponse);
    }
  }

  /**
   * Get transaction contracts info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getContracts(Tx tx, TxResponse txResponse) {
    List<ContractResponse> contractResponses = getContractResponses(tx, txResponse);
    if (!CollectionUtils.isEmpty(contractResponses)) {
      txResponse.setContracts(contractResponses);
    }
  }

  private List<ContractResponse> getContractResponses(Tx tx, TxResponse txResponse) {
    List<ContractResponse> contractResponses;
    List<TxContractProjection> txContractProjections;
    if (Boolean.TRUE.equals(tx.getValidContract())) {
      contractResponses =
          redeemerRepository.findContractByTx(tx).stream()
              .map(txContractMapper::fromTxContractProjectionToContractResponse)
              .toList();
      txContractProjections = txOutRepository.getContractDatumOutByTx(tx);
    } else {
      contractResponses =
          redeemerRepository.findContractByTxFail(tx).stream()
              .map(txContractMapper::fromTxContractProjectionToContractResponse)
              .toList();
      txContractProjections = txOutRepository.getContractDatumOutByTxFail(tx);
    }

    Map<String, TxContractProjection> txContractMap =
        txContractProjections.stream()
            .collect(
                Collectors.groupingBy(
                    TxContractProjection::getAddress,
                    Collectors.collectingAndThen(Collectors.toList(), list -> list.get(0))));

    List<ReferenceInputProjection> referenceInputProjections =
        referenceTxInRepository.getReferenceInputByTx(tx);
    List<TxReferenceInput> txReferenceInputsWithSmartContract = new ArrayList<>();
    List<TxReferenceInput> txReferenceInputsWithoutSmartContract = new ArrayList<>();
    referenceInputProjections.forEach(
        projection -> {
          if (Objects.nonNull(projection.getScriptHash())) {
            txReferenceInputsWithSmartContract.add(
                txReferenceInputMapper.fromReferenceInputProjectionTxReferenceInput(projection));
          } else {
            txReferenceInputsWithoutSmartContract.add(
                txReferenceInputMapper.fromReferenceInputProjectionTxReferenceInput(projection));
          }
        });

    Map<String, List<TxReferenceInput>> txReferenceInputSmartContractMap =
        txReferenceInputsWithSmartContract.stream()
            .collect(Collectors.groupingBy(TxReferenceInput::getScriptHash));

    contractResponses.parallelStream()
        .forEach(
            contractResponse -> {
              TxContractProjection txContractProjection =
                  txContractMap.get(contractResponse.getAddress());
              setContractReferenceInput(
                  contractResponse,
                  txReferenceInputSmartContractMap,
                  txReferenceInputsWithoutSmartContract);

              if (txContractProjection != null) {
                contractResponse.setDatumBytesOut(
                    txContractMapper.bytesToString(txContractProjection.getDatumBytesOut()));
                contractResponse.setDatumHashOut(txContractProjection.getDatumHashOut());
              }

              switch (contractResponse.getPurpose()) {
                case MINT -> setMintContractResponse(tx, contractResponse, txResponse);
                case SPEND -> setSpendContractResponse(contractResponse);
                case REWARD -> setRewardContractResponse(txResponse, contractResponse);
                case CERT -> setCertContractResponse(contractResponse);
                case VOTING -> setVoteContractResponse(contractResponse);
                case PROPOSING -> setProposeContractResponse(contractResponse);
              }
            });
    return contractResponses;
  }

  private void setVoteContractResponse(ContractResponse contractResponse) {
    // TODO
  }

  private void setProposeContractResponse(ContractResponse contractResponse) {
    // TODO
  }

  /**
   * Set cert contract response:
   *
   * @param contractResponse
   */
  private void setCertContractResponse(ContractResponse contractResponse) {
    contractResponse.setExecutionInputs(List.of(contractResponse.getStakeAddress()));
    contractResponse.setExecutionOutputs(List.of(contractResponse.getStakeAddress()));
  }

  /**
   * Set reward contract response
   *
   * @param txResponse
   * @param contractResponse
   */
  private void setRewardContractResponse(TxResponse txResponse, ContractResponse contractResponse) {
    contractResponse.setExecutionInputs(List.of(contractResponse.getStakeAddress()));
    contractResponse.setExecutionOutputs(
        txResponse.getUTxOs().getOutputs().stream().map(TxOutResponse::getAddress).toList());
  }

  /**
   * Set spend contract response
   *
   * @param contractResponse
   */
  private void setSpendContractResponse(ContractResponse contractResponse) {
    contractResponse.setExecutionInputs(List.of(contractResponse.getAddress()));
  }

  /**
   * Set contract reference input
   *
   * @param contractResponse
   * @param txReferenceInputSmartContractMap
   * @param txReferenceInputsWithoutSmartContract
   */
  private void setContractReferenceInput(
      ContractResponse contractResponse,
      Map<String, List<TxReferenceInput>> txReferenceInputSmartContractMap,
      List<TxReferenceInput> txReferenceInputsWithoutSmartContract) {
    if (txReferenceInputSmartContractMap.containsKey(contractResponse.getScriptHash())) {
      List<TxReferenceInput> txReferenceInputs =
          new ArrayList<>(txReferenceInputSmartContractMap.get(contractResponse.getScriptHash()));
      txReferenceInputs.addAll(txReferenceInputsWithoutSmartContract);
      contractResponse.setReferenceInputs(txReferenceInputs);
    } else {
      contractResponse.setReferenceInputs(txReferenceInputsWithoutSmartContract);
    }
  }

  /**
   * Set mint contract response
   *
   * @param tx
   * @param contractResponse
   */
  private void setMintContractResponse(
      Tx tx, ContractResponse contractResponse, TxResponse txResponse) {
    List<TokenAddressResponse> addressTokenProjections =
        multiAssetRepository.getMintingAssets(tx, contractResponse.getScriptHash()).stream()
            .map(tokenMapper::fromAddressTokenProjection)
            .toList();

    // set metadata for tokens
    Set<String> subjects =
        addressTokenProjections.stream()
            .map(
                tokenAddressResponse ->
                    contractResponse.getScriptHash() + tokenAddressResponse.getName())
            .collect(Collectors.toSet());
    Map<String, AssetMetadata> assetMetadataMap =
        assetMetadataRepository.findBySubjectIn(subjects).stream()
            .collect(Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    addressTokenProjections.forEach(
        tokenAddressResponse ->
            tokenAddressResponse.setMetadata(
                assetMetadataMapper.fromAssetMetadata(
                    assetMetadataMap.get(
                        contractResponse.getScriptHash() + tokenAddressResponse.getName()))));

    // set mint or burn tokens for contract response
    contractResponse.setMintingTokens(
        addressTokenProjections.stream()
            .filter(tokenAddressResponse -> tokenAddressResponse.getQuantity().longValue() >= 0)
            .toList());
    contractResponse.setBurningTokens(
        addressTokenProjections.stream()
            .filter(tokenAddressResponse -> tokenAddressResponse.getQuantity().longValue() < 0)
            .toList());

    List<String> tokenMintedByContract =
        addressTokenProjections.stream().map(TokenAddressResponse::getFingerprint).toList();

    List<String> executionOutputs =
        Stream.concat(
                txResponse.getUTxOs().getInputs().stream(),
                txResponse.getUTxOs().getOutputs().stream())
            .filter(
                txOutResponse ->
                    !Collections.disjoint(
                        tokenMintedByContract,
                        txOutResponse.getTokens().stream()
                            .map(TxMintingResponse::getAssetId)
                            .toList()))
            .map(TxOutResponse::getAddress)
            .toList();

    contractResponse.setExecutionOutputs(executionOutputs);
  }

  /**
   * Get transaction summary and UTxOs info
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getSummaryAndUTxOs(Tx tx, TxResponse txResponse) {
    List<AddressInputOutputProjection> addressInputInfo = txOutRepository.getTxAddressInputInfo(tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap =
        addressInputInfo.stream()
            .collect(
                Collectors.groupingBy(
                    txOutMapper::fromAddressInputOutput, LinkedHashMap::new, Collectors.toList()));
    List<AddressInputOutputProjection> addressOutputInfo =
        txOutRepository.getTxAddressOutputInfo(tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap =
        addressOutputInfo.stream()
            .collect(
                Collectors.groupingBy(
                    txOutMapper::fromAddressInputOutput, LinkedHashMap::new, Collectors.toList()));

    // Get metadata
    List<Long> multiAssetIdList = new ArrayList<>();
    multiAssetIdList.addAll(
        addressInputInfo.stream()
            .map(AddressInputOutputProjection::getMultiAssetId)
            .collect(Collectors.toSet()));
    multiAssetIdList.addAll(
        addressOutputInfo.stream()
            .map(AddressInputOutputProjection::getMultiAssetId)
            .collect(Collectors.toSet()));

    Pair<Map<String, AssetMetadata>, Map<Long, MultiAsset>> getMapMetadataAndMapAsset =
        getMapMetadataAndMapAsset(multiAssetIdList);

    // uTxO
    List<TxOutResponse> uTxOOutputs =
        mappingProjectionToAddressWithMetadata(
            addressOutputMap,
            getMapMetadataAndMapAsset.getFirst(),
            getMapMetadataAndMapAsset.getSecond());
    List<TxOutResponse> uTxOInputs =
        mappingProjectionToAddressWithMetadata(
            addressInputMap,
            getMapMetadataAndMapAsset.getFirst(),
            getMapMetadataAndMapAsset.getSecond());

    UTxOResponse uTxOs = UTxOResponse.builder().inputs(uTxOInputs).outputs(uTxOOutputs).build();
    txResponse.setUTxOs(uTxOs);

    // Summary
    List<TxOutResponse> addressesInfoInput = getStakeAddressInfo(addressInputInfo);
    List<TxOutResponse> addressesInfoOutput = getStakeAddressInfo(addressOutputInfo);

    List<TxOutResponse> stakeAddress =
        removeDuplicateTx(
            addressesInfoInput,
            addressesInfoOutput,
            getMapMetadataAndMapAsset.getFirst(),
            getMapMetadataAndMapAsset.getSecond());

    SummaryResponse summary = SummaryResponse.builder().stakeAddress(stakeAddress).build();

    txResponse.setSummary(summary);
  }

  private List<TxOutResponse> removeDuplicateTx(
      List<TxOutResponse> addressesInputs,
      List<TxOutResponse> addressesOutputs,
      Map<String, AssetMetadata> assetMetadataMap,
      Map<Long, MultiAsset> multiAssetMap) {

    addressesInputs.forEach(
        txOutResponse -> {
          txOutResponse.setValue(txOutResponse.getValue().multiply(BigInteger.valueOf(-1)));
          txOutResponse
              .getTokens()
              .forEach(
                  token ->
                      token.setAssetQuantity(
                          token.getAssetQuantity().multiply(BigInteger.valueOf(-1))));
        });

    Map<String, List<TxOutResponse>> unionTxsByAddress =
        Stream.concat(addressesInputs.stream(), addressesOutputs.stream())
            .collect(
                Collectors.groupingBy(
                    TxOutResponse::getAddress, Collectors.toCollection(ArrayList::new)));

    List<TxOutResponse> summary = new ArrayList<>();

    unionTxsByAddress.forEach(
        (address, txs) -> {
          TxOutResponse txOutResponse = new TxOutResponse();
          BigInteger totalValue =
              txs.stream().map(TxOutResponse::getValue).reduce(BigInteger.ZERO, BigInteger::add);
          Map<String, List<TxMintingResponse>> unionTokenByAsset =
              txs.stream()
                  .map(TxOutResponse::getTokens)
                  .flatMap(List::stream)
                  .collect(
                      Collectors.groupingBy(
                          TxMintingResponse::getAssetId, Collectors.toCollection(ArrayList::new)));

          List<TxMintingResponse> tokenResponse = new ArrayList<>();

          unionTokenByAsset.forEach(
              (asset, tokens) -> {
                BigInteger totalQuantity =
                    tokens.stream()
                        .map(TxMintingResponse::getAssetQuantity)
                        .reduce(BigInteger.ZERO, BigInteger::add);

                if (!BigInteger.ZERO.equals(totalQuantity)) {
                  TxMintingResponse token = tokens.get(0);
                  setMetadata(assetMetadataMap, multiAssetMap, token);
                  token.setAssetQuantity(totalQuantity);
                  tokenResponse.add(token);
                }
              });
          txOutResponse.setAddress(address);
          txOutResponse.setTokens(tokenResponse);
          txOutResponse.setValue(totalValue);
          summary.add(txOutResponse);
        });

    return summary.stream()
        .sorted(Comparator.comparing(TxOutResponse::getValue))
        .collect(Collectors.toList());
  }

  private void setMetadata(
      Map<String, AssetMetadata> assetMetadataMap,
      Map<Long, MultiAsset> multiAssetMap,
      TxMintingResponse token) {
    MultiAsset multiAsset = multiAssetMap.get(token.getMultiAssetId());
    if (!Objects.isNull(multiAsset)) {
      String subject = multiAsset.getPolicy() + multiAsset.getName();
      AssetMetadata metadata = assetMetadataMap.get(subject);
      token.setMetadata(assetMetadataMapper.fromAssetMetadata(metadata));
    }
  }

  /**
   * Get transaction stake certificates info
   *
   * @param tx transaction
   * @param txResponse response data of stake certificates
   */
  private void getStakeCertificates(Tx tx, TxResponse txResponse) {
    List<TxStakeCertificate> stakeCertificates =
        stakeRegistrationRepository.findByTx(tx).stream()
            .map(
                item ->
                    new TxStakeCertificate(
                        item.getAddr().getView(), CertificateType.STAKE_REGISTRATION))
            .collect(Collectors.toList());
    stakeCertificates.addAll(
        stakeDeRegistrationRepository.findByTx(tx).stream()
            .map(
                item ->
                    new TxStakeCertificate(
                        item.getAddr().getView(), CertificateType.STAKE_DEREGISTRATION))
            .collect(Collectors.toList()));
    if (!CollectionUtils.isEmpty(stakeCertificates)) {
      txResponse.setStakeCertificates(stakeCertificates);
    }
  }

  /**
   * Get transaction pool certificates info
   *
   * @param tx transaction
   * @param txResponse response data of pool certificates
   */
  private void getPoolCertificates(Tx tx, TxResponse txResponse) {

    // get pool registration
    List<PoolUpdateDetailProjection> poolUpdateDetailProjections =
        poolUpdateRepository.findByTx(tx);
    Set<Long> poolUpdateIdSet =
        poolUpdateDetailProjections.stream()
            .map(PoolUpdateDetailProjection::getPoolUpdateId)
            .collect(Collectors.toSet());
    List<StakeKeyProjection> poolOwners =
        poolUpdateRepository.findOwnerAccountByPoolUpdate(poolUpdateIdSet);
    Map<Long, List<String>> poolOwnerMap =
        poolOwners.stream()
            .collect(
                Collectors.groupingBy(
                    StakeKeyProjection::getPoolUpdateId,
                    Collectors.mapping(StakeKeyProjection::getView, Collectors.toList())));
    List<PoolRelayProjection> poolRelays = poolRelayRepository.findByPoolHashIdIn(poolUpdateIdSet);
    Map<Long, List<PoolRelayProjection>> poolRelayMap =
        poolRelays.stream().collect(Collectors.groupingBy(PoolRelayProjection::getPoolUpdateId));
    List<TxPoolCertificate> poolCertificates =
        poolUpdateDetailProjections.stream()
            .map(
                item -> {
                  List<PoolRelayResponse> relays = null;
                  if (poolRelayMap.containsKey(item.getPoolUpdateId())) {
                    relays =
                        poolRelayMap.get(item.getPoolUpdateId()).stream()
                            .map(
                                relay ->
                                    PoolRelayResponse.builder()
                                        .dnsName(relay.getDnsName())
                                        .dnsSrvName(relay.getDnsSrvName())
                                        .ipv4(relay.getIpv4())
                                        .ipv6(relay.getIpv6())
                                        .port(relay.getPort())
                                        .build())
                            .collect(Collectors.toList());
                  }
                  List<String> poolOwnersList = new ArrayList<>();
                  if (poolOwnerMap.containsKey(item.getPoolUpdateId())) {
                    poolOwnersList = poolOwnerMap.get(item.getPoolUpdateId());
                  }
                  return TxPoolCertificate.builder()
                      .poolId(item.getPoolView())
                      .margin(item.getMargin())
                      .rewardAccount(item.getRewardAccount())
                      .pledge(item.getPledge())
                      .vrfKey(item.getVrfKey())
                      .cost(item.getCost())
                      .metadataHash(item.getMetadataHash())
                      .metadataUrl(item.getMetadataUrl())
                      .poolOwners(poolOwnersList)
                      .relays(relays)
                      .type(CertificateType.POOL_REGISTRATION)
                      .build();
                })
            .collect(Collectors.toList());

    // get pool de registration
    List<PoolDeRegistrationProjection> poolRetireDetailProjections =
        poolRetireRepository.findByAnnouncedTx(tx);
    poolCertificates.addAll(
        poolRetireDetailProjections.stream()
            .map(
                item ->
                    TxPoolCertificate.builder()
                        .poolId(item.getPoolId())
                        .epoch(item.getRetiringEpoch())
                        .type(CertificateType.POOL_DEREGISTRATION)
                        .build())
            .collect(Collectors.toList()));
    if (!CollectionUtils.isEmpty(poolCertificates)) {
      txResponse.setPoolCertificates(poolCertificates);
    }
  }

  /**
   * Map data from AddressInputOutputProjection to TxOutResponse
   *
   * @param addressInputOutputMap address with metadata projection map
   * @return address response
   */
  private List<TxOutResponse> mappingProjectionToAddressWithMetadata(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap,
      Map<String, AssetMetadata> assetMetadataMap,
      Map<Long, MultiAsset> multiAssetMap) {
    List<TxOutResponse> uTxOs = new ArrayList<>(addressInputOutputMap.keySet());
    for (TxOutResponse uTxO : uTxOs) {
      List<TxMintingResponse> tokens =
          addressInputOutputMap.get(uTxO).stream()
              .filter(token -> Objects.nonNull(token.getAssetId()))
              .map(maTxMintMapper::fromAddressInputOutputProjection)
              .collect(Collectors.toList());
      tokens.addAll(getTokenInFailedTxOut(addressInputOutputMap, uTxO));
      tokens.forEach(token -> setMetadata(assetMetadataMap, multiAssetMap, token));
      uTxO.setTokens(tokens);
    }
    return uTxOs;
  }

  /**
   * Map data from AddressInputOutputProjection to TxOutResponse
   *
   * @param addressInputOutputMap address projection map
   * @return address response
   */
  private List<TxOutResponse> mappingProjectionToAddress(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap) {
    List<TxOutResponse> uTxOs = new ArrayList<>(addressInputOutputMap.keySet());
    for (TxOutResponse uTxO : uTxOs) {
      List<TxMintingResponse> tokens =
          addressInputOutputMap.get(uTxO).stream()
              .filter(token -> Objects.nonNull(token.getAssetId()))
              .map(maTxMintMapper::fromAddressInputOutputProjection)
              .collect(Collectors.toList());
      tokens.addAll(getTokenInFailedTxOut(addressInputOutputMap, uTxO));
      uTxO.setTokens(tokens);
    }
    return uTxOs;
  }

  /**
   * If data from collateral output, handle token in json string data Otherwise, not exist json
   * string data Example of json string data:
   * [{"unit":"LOVELACE","policyId":"","assetName":"TE9WRUxBQ0U=","quantity":2726335}]
   *
   * @param addressInputOutputMap address projection map
   * @param txOut list collateral output
   */
  private List<TxMintingResponse> getTokenInFailedTxOut(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap,
      TxOutResponse txOut) {
    List<TxMintingResponse> tokens = new ArrayList<>();
    List<String> tokenStringList =
        addressInputOutputMap.get(txOut).stream()
            .map(AddressInputOutputProjection::getAssetsJson)
            .filter(StringUtils::isNotEmpty)
            .toList();
    ObjectMapper objectMapper = new ObjectMapper();
    tokenStringList.forEach(
        tokenString -> {
          if (StringUtils.isNotEmpty(tokenString)) {
            try {
              JsonNode tokenFailedTxOut = objectMapper.readValue(tokenString, JsonNode.class);
              for (JsonNode token : tokenFailedTxOut) {
                String[] policyAndName = token.get("unit").asText().split("\\.");
                if (CommonConstant.LOVELACE.equals(token.get("unit").asText())
                    || policyAndName.length < 2) {
                  continue;
                }
                String assetName = policyAndName[1];
                String assetDisplayName =
                    HexUtils.fromHex(
                        assetName,
                        AssetUtil.calculateFingerPrint(token.get("policyId").asText(), assetName));
                TxMintingResponse txMintingResponse =
                    TxMintingResponse.builder()
                        .assetId(token.get("unit").asText())
                        .assetName(assetDisplayName)
                        .policy(token.get("policyId").asText())
                        .assetQuantity(new BigInteger(token.get("quantity").asText()))
                        .build();
                tokens.add(txMintingResponse);
              }
            } catch (JsonProcessingException e) {
              log.error("Failed to parse token json string: {}", tokenString);
            }
          }
        });
    return tokens;
  }

  /**
   * Get stake address info from address
   *
   * @param addressInfo List address input or output info
   * @return list stake address input or output info
   */
  private List<TxOutResponse> getStakeAddressInfo(List<AddressInputOutputProjection> addressInfo) {

    Map<TxOutResponse, List<AddressInputOutputProjection>> addressMap =
        addressInfo.stream()
            .collect(Collectors.groupingBy(txOutMapper::fromStakeAddressInputOutput));
    Map<String, List<AddressInputOutputProjection>> addressTokenMap =
        addressInfo.stream()
            .collect(Collectors.groupingBy(AddressInputOutputProjection::getStakeAddress));
    var addressValueMap =
        addressMap.keySet().stream()
            .collect(
                Collectors.groupingBy(
                    TxOutResponse::getAddress,
                    Collectors.reducing(
                        BigInteger.ZERO, TxOutResponse::getValue, BigInteger::add)));
    List<TxOutResponse> stakeAddressTxInputList = new ArrayList<>();
    addressValueMap.forEach(
        (key, value) ->
            stakeAddressTxInputList.add(
                TxOutResponse.builder()
                    .address(key)
                    .value(value)
                    .tokens(
                        addressTokenMap.get(key).stream()
                            .filter(token -> Objects.nonNull(token.getAssetId()))
                            .map(maTxMintMapper::fromAddressInputOutputProjection)
                            .collect(Collectors.toList()))
                    .build()));
    return stakeAddressTxInputList;
  }

  /**
   * get transaction graph in month with day unit
   *
   * @param day
   * @return
   */
  private List<TxGraph> getTransactionChartInRange(long day) {
    final String key = getRedisKey(TRANSACTION_GRAPH_MONTH_KEY);
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLocalDate =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final LocalDateTime previousMonth =
        currentLocalDate
            .minusMonths(BigInteger.ONE.longValue())
            .minusDays(BigInteger.ONE.longValue());

    final BigInteger previousMonthInSeconds =
        BigInteger.valueOf(previousMonth.toInstant(ZoneOffset.UTC).getEpochSecond());

    long maxDay = ChronoUnit.DAYS.between(previousMonth, currentLocalDate);

    if (maxDay < day) {
      day = maxDay;
    }

    var keySize = redisTemplate.opsForList().size(key);
    // if key not exists or empty
    if (Objects.isNull(keySize) || keySize.equals(BigInteger.ZERO.longValue())) {
      List<TxGraph> txCharts =
          toTxGraph(txChartRepository.getTransactionGraphDayGreaterThan(previousMonthInSeconds));
      updateRedisTxGraph(txCharts, key, Boolean.TRUE);

      return getTxGraphsByDayRange(
          (int) day,
          txCharts.stream().sorted(Comparator.comparing(TxGraph::getDate).reversed()).toList());
    }

    // txGraphsRedis will not empty
    List<TxGraph> txGraphsRedis =
        redisTemplate.opsForList().range(key, BigInteger.ZERO.longValue(), DAYS_IN_MONTH);

    final var latestDay = txGraphsRedis.get(BigInteger.ZERO.intValue()).getDate();
    final var latestLocalDateTime =
        Instant.ofEpochMilli(latestDay.getTime()).atZone(ZoneOffset.UTC).toLocalDateTime();

    while (ChronoUnit.DAYS.between(
            LocalDateTime.ofInstant(
                txGraphsRedis.get(txGraphsRedis.size() - 1).getDate().toInstant(), ZoneOffset.UTC),
            currentLocalDate)
        > DAYS_IN_MONTH) {
      redisTemplate.opsForList().rightPop(key);
      txGraphsRedis.remove(txGraphsRedis.size() - BigInteger.ONE.intValue());
    }

    var distanceFromRealAndCache = ChronoUnit.DAYS.between(latestLocalDateTime, currentLocalDate);

    if (distanceFromRealAndCache >= BigInteger.TWO.longValue()) {
      final long dayMissingData = day - distanceFromRealAndCache;
      if (dayMissingData <= BigInteger.ZERO.longValue()) {
        return Collections.emptyList();
      }

      List<TxGraph> txCharts =
          toTxGraph(txChartRepository.getTransactionGraphDayGreaterThan(previousMonthInSeconds));
      updateRedisTxGraph(txCharts, key, Boolean.TRUE);
      return getTxGraphsByDayRange(
          (int) (day - distanceFromRealAndCache),
          txCharts.stream().sorted(Comparator.comparing(TxGraph::getDate).reversed()).toList());
    }
    // get 2 days: today and yesterday
    List<BigInteger> days =
        LongStream.range(BigInteger.ZERO.intValue(), BigInteger.TWO.longValue())
            .boxed()
            .map(
                dayMinus -> {
                  var markDay = currentLocalDate.minusDays(dayMinus);
                  return BigInteger.valueOf(markDay.toInstant(ZoneOffset.UTC).getEpochSecond());
                })
            .toList();

    List<TxGraph> txs = toTxGraph(txChartRepository.getTransactionGraphByDay(days));

    if (ObjectUtils.isEmpty(txs)) {
      return getTxGraphsByDayRange((int) day, txGraphsRedis);
    }

    final var txGraphs = new ArrayList<TxGraph>();
    // this mean that there are no transaction in latest day at time this api was call
    if (txs.size() == BigInteger.ONE.intValue()) {
      return getTxGraphsByDayRange((int) day, txGraphsRedis);
    }

    var latestChart = txs.get(BigInteger.ONE.intValue());
    var previousChart = txs.get(BigInteger.ZERO.intValue());

    if (distanceFromRealAndCache == BigInteger.ONE.longValue()
        && txs.size() == BigInteger.TWO.intValue()) {
      redisTemplate.opsForList().set(key, BigInteger.ZERO.intValue(), previousChart);
      redisTemplate.opsForList().leftPush(key, latestChart);

      txGraphs.add(latestChart);
      txGraphs.add(previousChart);
      txGraphs.addAll(txGraphsRedis.subList(BigInteger.ONE.intValue(), txGraphsRedis.size()));

      return getTxGraphsByDayRange((int) day, txGraphs);
    }

    txGraphs.add(latestChart);
    txGraphs.add(previousChart);
    txGraphs.addAll(txGraphsRedis.subList(BigInteger.TWO.intValue(), txGraphsRedis.size()));

    return getTxGraphsByDayRange((int) day, txGraphs);
  }

  private static List<TxGraph> getTxGraphsByDayRange(int day, List<TxGraph> txCharts) {

    if (txCharts.size() < day) {
      return txCharts.stream().sorted(Comparator.comparing(TxGraph::getDate)).toList();
    }

    return txCharts.subList(BigInteger.ZERO.intValue(), day).stream()
        .sorted(Comparator.comparing(TxGraph::getDate))
        .toList();
  }

  /**
   * sum transaction in 24 hours in order to draw graph
   *
   * @return
   */
  private List<TxGraph> getTransactionChartInOneDay() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(localDate.getHour(), BigInteger.ZERO.intValue()));

    List<BigInteger> hours =
        LongStream.range(BigInteger.ZERO.intValue(), HOURS_IN_DAY)
            .boxed()
            .map(currentLoaLocalDateTime::minusHours)
            .map(hour -> BigInteger.valueOf(hour.toInstant(ZoneOffset.UTC).getEpochSecond()))
            .toList();

    List<TxGraphProjection> txs = txChartRepository.getTransactionGraphByHour(hours);

    if (CollectionUtils.isEmpty(txs)) {
      return Collections.emptyList();
    }

    return toTxGraph(txs).stream().sorted(Comparator.comparing(TxGraph::getDate)).toList();
  }

  /**
   * Mapping projection to TxGraph
   *
   * @param txs
   * @return
   */
  private static List<TxGraph> toTxGraph(List<TxGraphProjection> txs) {
    return txs.stream()
        .map(
            txChart ->
                TxGraph.builder()
                    .date(
                        Date.from(
                            OffsetDateTime.ofInstant(
                                    Instant.ofEpochSecond(txChart.getTime().longValue()),
                                    ZoneOffset.UTC)
                                .toInstant()))
                    .simpleTransactions(txChart.getSimpleTransactions())
                    .smartContract(txChart.getSmartContract())
                    .metadata(txChart.getMetadata())
                    .build())
        .toList();
  }

  /**
   * Update redis index
   *
   * @param txGraphs
   * @param key
   * @param isEmpty
   */
  private void updateRedisTxGraph(List<TxGraph> txGraphs, String key, boolean isEmpty) {

    if (isEmpty) {
      txGraphs.forEach(txGraph -> redisTemplate.opsForList().leftPush(key, txGraph));
    }
  }

  /**
   * create redis key
   *
   * @param rawKey
   * @return
   */
  private String getRedisKey(String rawKey) {
    return String.join("_", this.network, rawKey);
  }

  /**
   * Get protocol params update in transaction
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getProtocols(Tx tx, TxResponse txResponse) {

    List<ParamProposal> paramProposals =
        paramProposalRepository.getParamProposalByRegisteredTxId(tx.getId());

    if (!ObjectUtils.isEmpty(paramProposals)) {
      // find current change param
      txResponse.setProtocols(protocolMapper.mapProtocolParamResponse(paramProposals));
      // get previous value
      if (Objects.nonNull(txResponse.getProtocols())) {
        Integer epochNo = tx.getBlock().getEpochNo();

        epochParamRepository
            .findEpochParamByEpochNo(epochNo)
            .ifPresent(
                epochParam ->
                    txResponse.setPreviousProtocols(
                        protocolMapper.mapPreviousProtocolParamResponse(
                            epochParam, txResponse.getProtocols())));
      }
      processCheckIfSameProtocol(txResponse);
    }
  }

  private void processCheckIfSameProtocol(TxResponse txResponse) {
    ProtocolParamResponse currentProtocol = txResponse.getProtocols();
    ProtocolParamResponse previousProtocol = txResponse.getPreviousProtocols();

    if (Objects.nonNull(currentProtocol.getMinFeeA())
        && Objects.hashCode(currentProtocol.getMinFeeA())
            == Objects.hashCode(previousProtocol.getMinFeeA())) {
      currentProtocol.setMinFeeA(null);
      previousProtocol.setMinFeeA(null);
    }

    if (Objects.nonNull(currentProtocol.getMinFeeB())
        && Objects.hashCode(currentProtocol.getMinFeeB())
            == Objects.hashCode(previousProtocol.getMinFeeB())) {
      currentProtocol.setMinFeeB(null);
      previousProtocol.setMinFeeB(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockSize())
        && Objects.hashCode(currentProtocol.getMaxBlockSize())
            == Objects.hashCode(previousProtocol.getMaxBlockSize())) {
      currentProtocol.setMaxBlockSize(null);
      previousProtocol.setMaxBlockSize(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxSize())
        && Objects.hashCode(currentProtocol.getMaxTxSize())
            == Objects.hashCode(previousProtocol.getMaxTxSize())) {
      currentProtocol.setMaxTxSize(null);
      previousProtocol.setMaxTxSize(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBhSize())
        && Objects.hashCode(currentProtocol.getMaxBhSize())
            == Objects.hashCode(previousProtocol.getMaxBhSize())) {
      currentProtocol.setMaxBhSize(null);
      previousProtocol.setMaxBhSize(null);
    }

    if (Objects.nonNull(currentProtocol.getKeyDeposit())
        && Objects.hashCode(currentProtocol.getKeyDeposit())
            == Objects.hashCode(previousProtocol.getKeyDeposit())) {
      currentProtocol.setKeyDeposit(null);
      previousProtocol.setKeyDeposit(null);
    }

    if (Objects.nonNull(currentProtocol.getPoolDeposit())
        && Objects.hashCode(currentProtocol.getPoolDeposit())
            == Objects.hashCode(previousProtocol.getPoolDeposit())) {
      currentProtocol.setPoolDeposit(null);
      previousProtocol.setPoolDeposit(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxEpoch())
        && Objects.hashCode(currentProtocol.getMaxEpoch())
            == Objects.hashCode(previousProtocol.getMaxEpoch())) {
      currentProtocol.setMaxEpoch(null);
      previousProtocol.setMaxEpoch(null);
    }

    if (Objects.nonNull(currentProtocol.getOptimalPoolCount())
        && Objects.hashCode(currentProtocol.getOptimalPoolCount())
            == Objects.hashCode(previousProtocol.getOptimalPoolCount())) {
      currentProtocol.setOptimalPoolCount(null);
      previousProtocol.setOptimalPoolCount(null);
    }

    if (Objects.nonNull(currentProtocol.getMinUtxoValue())
        && Objects.hashCode(currentProtocol.getMinUtxoValue())
            == Objects.hashCode(previousProtocol.getMinUtxoValue())) {
      currentProtocol.setMinUtxoValue(null);
      previousProtocol.setMinUtxoValue(null);
    }

    if (Objects.nonNull(currentProtocol.getMinPoolCost())
        && Objects.hashCode(currentProtocol.getMinPoolCost())
            == Objects.hashCode(previousProtocol.getMinPoolCost())) {
      currentProtocol.setMinPoolCost(null);
      previousProtocol.setMinPoolCost(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxExMem())
        && Objects.hashCode(currentProtocol.getMaxTxExMem())
            == Objects.hashCode(previousProtocol.getMaxTxExMem())) {
      currentProtocol.setMaxTxExMem(null);
      previousProtocol.setMaxTxExMem(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxExSteps())
        && Objects.hashCode(currentProtocol.getMaxTxExSteps())
            == Objects.hashCode(previousProtocol.getMaxTxExSteps())) {
      currentProtocol.setMaxTxExSteps(null);
      previousProtocol.setMaxTxExSteps(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockExMem())
        && Objects.hashCode(currentProtocol.getMaxBlockExMem())
            == Objects.hashCode(previousProtocol.getMaxBlockExMem())) {
      currentProtocol.setMaxBlockExMem(null);
      previousProtocol.setMaxBlockExMem(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockExSteps())
        && Objects.hashCode(currentProtocol.getMaxBlockExSteps())
            == Objects.hashCode(previousProtocol.getMaxBlockExSteps())) {
      currentProtocol.setMaxBlockExSteps(null);
      previousProtocol.setMaxBlockExSteps(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxValSize())
        && Objects.hashCode(currentProtocol.getMaxValSize())
            == Objects.hashCode(previousProtocol.getMaxValSize())) {
      currentProtocol.setMaxValSize(null);
      previousProtocol.setMaxValSize(null);
    }

    if (Objects.nonNull(currentProtocol.getCoinsPerUtxoSize())
        && Objects.hashCode(currentProtocol.getCoinsPerUtxoSize())
            == Objects.hashCode(previousProtocol.getCoinsPerUtxoSize())) {
      currentProtocol.setCoinsPerUtxoSize(null);
      previousProtocol.setCoinsPerUtxoSize(null);
    }

    if (Objects.nonNull(currentProtocol.getInfluence())
        && Objects.hashCode(currentProtocol.getInfluence())
            == Objects.hashCode(previousProtocol.getInfluence())) {
      currentProtocol.setInfluence(null);
      previousProtocol.setInfluence(null);
    }

    if (Objects.nonNull(currentProtocol.getMonetaryExpandRate())
        && Objects.hashCode(currentProtocol.getMonetaryExpandRate())
            == Objects.hashCode(previousProtocol.getMonetaryExpandRate())) {
      currentProtocol.setMonetaryExpandRate(null);
      previousProtocol.setMonetaryExpandRate(null);
    }

    if (Objects.nonNull(currentProtocol.getTreasuryGrowthRate())
        && Objects.hashCode(currentProtocol.getTreasuryGrowthRate())
            == Objects.hashCode(previousProtocol.getTreasuryGrowthRate())) {
      currentProtocol.setTreasuryGrowthRate(null);
      previousProtocol.setTreasuryGrowthRate(null);
    }

    if (Objects.nonNull(currentProtocol.getDecentralisation())
        && Objects.hashCode(currentProtocol.getDecentralisation())
            == Objects.hashCode(previousProtocol.getDecentralisation())) {
      currentProtocol.setDecentralisation(null);
      previousProtocol.setDecentralisation(null);
    }

    if (Objects.nonNull(currentProtocol.getPriceMem())
        && Objects.hashCode(currentProtocol.getPriceMem())
            == Objects.hashCode(previousProtocol.getPriceMem())) {
      currentProtocol.setPriceMem(null);
      previousProtocol.setPriceMem(null);
    }

    if (Objects.nonNull(currentProtocol.getPriceStep())
        && Objects.hashCode(currentProtocol.getPriceStep())
            == Objects.hashCode(previousProtocol.getPriceStep())) {
      currentProtocol.setPriceStep(null);
      previousProtocol.setPriceStep(null);
    }

    if (Objects.nonNull(currentProtocol.getProtocolMajor())
        && Objects.hashCode(currentProtocol.getProtocolMajor())
            == Objects.hashCode(previousProtocol.getProtocolMajor())
        && Objects.nonNull(currentProtocol.getProtocolMinor())
        && Objects.hashCode(currentProtocol.getProtocolMinor())
            == Objects.hashCode(previousProtocol.getProtocolMinor())) {
      currentProtocol.setProtocolMajor(null);
      previousProtocol.setProtocolMajor(null);
      currentProtocol.setProtocolMinor(null);
      previousProtocol.setProtocolMinor(null);
    }

    if (Objects.nonNull(currentProtocol.getCollateralPercent())
        && Objects.hashCode(currentProtocol.getCollateralPercent())
            == Objects.hashCode(previousProtocol.getCollateralPercent())) {
      currentProtocol.setCollateralPercent(null);
      previousProtocol.setCollateralPercent(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxCollateralInputs())
        && Objects.hashCode(currentProtocol.getMaxCollateralInputs())
            == Objects.hashCode(previousProtocol.getMaxCollateralInputs())) {
      currentProtocol.setMaxCollateralInputs(null);
      previousProtocol.setMaxCollateralInputs(null);
    }

    if (Objects.nonNull(currentProtocol.getEntropy())
        && Objects.hashCode(currentProtocol.getEntropy())
            == Objects.hashCode(previousProtocol.getEntropy())) {
      currentProtocol.setEntropy(null);
      previousProtocol.setEntropy(null);
    }

    if (Objects.nonNull(currentProtocol.getCostModel())
        && Objects.hashCode(currentProtocol.getCostModel())
            == Objects.hashCode(previousProtocol.getCostModel())) {
      currentProtocol.setCostModel(null);
      previousProtocol.setCostModel(null);
    }

    if (currentProtocol.isNull() && previousProtocol.isNull()) {
      currentProtocol = null;
      previousProtocol = null;
    }

    txResponse.setProtocols(currentProtocol);
    txResponse.setPreviousProtocols(previousProtocol);
  }
}
