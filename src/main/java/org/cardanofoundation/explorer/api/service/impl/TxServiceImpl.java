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
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
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

import org.cardanofoundation.explorer.api.model.response.tx.*;
import org.cardanofoundation.explorer.api.projection.TxProjection;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.bloxbean.cardano.client.util.AssetUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.DelegationMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.mapper.TxMapper;
import org.cardanofoundation.explorer.api.mapper.TxOutMapper;
import org.cardanofoundation.explorer.api.mapper.WithdrawalMapper;
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
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.FailedTxOutRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.PoolRelayRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.TxChartRepository;
import org.cardanofoundation.explorer.api.repository.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.repository.UnconsumeTxInRepository;
import org.cardanofoundation.explorer.api.repository.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AddressToken;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.springframework.data.util.Pair;

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
  private final StakeAddressRepository stakeAddressRepository;
  private final WithdrawalMapper withdrawalMapper;
  private final DelegationRepository delegationRepository;
  private final DelegationMapper delegationMapper;
  private final MaTxMintRepository maTxMintRepository;
  private final MaTxMintMapper maTxMintMapper;
  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final AddressTokenRepository addressTokenRepository;
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

  private final RedisTemplate<String, TxGraph> redisTemplate;
  private static final int SUMMARY_SIZE = 4;
  public static final long HOURS_IN_DAY = 24;
  public static final long DAY_IN_WEEK = 7;
  public static final long DAY_IN_TWO_WEEK = DAY_IN_WEEK * 2;
  public static final long DAYS_IN_MONTH = 32;

  private static final String TRANSACTION_GRAPH_MONTH_KEY = "TRANSACTION_GRAPH_MONTH";

  @Value("${application.network}")
  private String network;

  @Override
  @Transactional(readOnly = true)
  public List<TxSummary> findLatestTxSummary() {
    List<Long> txIds = txRepository.findLatestTxId(
        PageRequest.of(BigInteger.ZERO.intValue(),
            SUMMARY_SIZE,
            Sort.by(BaseEntity_.ID).descending()));

    if (txIds.isEmpty()) {
      return Collections.emptyList();
    }

    List<TxSummary> summaries = new ArrayList<>();
    List<TxIOProjection> txs = txRepository.findLatestTxIO(txIds);

    txs.forEach(tx -> {

      Optional<TxSummary> searchedSummary = summaries.stream().filter(summary ->
          summary.getHash().equals(tx.getHash())
      ).findFirst();

      if (searchedSummary.isEmpty()) {

        final var from = new ArrayList<String>();
        from.add(tx.getFromAddress());

        final var to = new ArrayList<String>();
        to.add(tx.getToAddress());

        final TxSummary summary = TxSummary.builder()
            .blockNo(tx.getBlockNo())
            .hash(tx.getHash())
            .amount(tx.getAmount().doubleValue())
            .fromAddress(from)
            .toAddress(to)
            .epochNo(tx.getEpochNo())
            .epochSlotNo(tx.getEpochSlotNo())
            .slot(tx.getSlot())
            .time(tx.getTime())
            .status(Boolean.TRUE.equals(tx.getValidContract()) ? TxStatus.SUCCESS : TxStatus.FAIL)
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
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getAll(Pageable pageable) {
    Page<Tx> txPage = txRepository.findAllTx(pageable);
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByBlock(String blockId,
                                                                     Pageable pageable) {
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
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByAddress(String address,
                                                                       Pageable pageable) {
    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND)
    );
    List<Tx> txList = addressTxBalanceRepository.findAllByAddress(addr, pageable);

    Page<Tx> txPage = new PageImpl<>(txList, pageable, addr.getTxCount());
    return new BaseFilterResponse<>(txPage, mapDataFromTxByAddressListToResponses(txPage, addr));
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByToken(String tokenId,
                                                                     Pageable pageable) {
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Optional<MultiAsset> multiAsset = multiAssetRepository.findByFingerprint(tokenId);
    if (multiAsset.isPresent()) {
      List<Long> txIds = addressTokenRepository.findTxsByMultiAsset(multiAsset.get(), pageable);
      List<Tx> txList = txRepository.findByIdIn(txIds);
      Page<Tx> txPage = new PageImpl<>(txList, pageable, multiAsset.get().getTxCount());
      List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage);
      response = new BaseFilterResponse<>(txPage, txFilterResponses);
    }
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByStake(String stakeKey,
                                                                     Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND)
    );

    Page<TxProjection> txPage =
        addressTxBalanceRepository.findAllByStakeId(stakeAddress.getId(), pageable);
    List<TxFilterResponse> data = mapDataFromTxByStakeListToResponseList(txPage);
    return new BaseFilterResponse<>(txPage, data);
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
    Set<Long> blockIdList = txPage.getContent().stream().map(Tx::getBlockId)
        .collect(Collectors.toSet());
    List<Block> blocks = blockRepository.findAllByIdIn(blockIdList);
    Map<Long, Block> blockMap = blocks.stream()
        .collect(Collectors.toMap(Block::getId, Function.identity()));
    //get addresses input
    Set<Long> txIdSet = txPage.getContent().stream().map(Tx::getId).collect(Collectors.toSet());
    List<AddressInputOutputProjection> txInList = txOutRepository.findAddressInputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    //get addresses output
    List<AddressInputOutputProjection> txOutList = txOutRepository.findAddressOutputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressOutMap = txOutList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (Tx tx : txPage.getContent()) {
      Long txId = tx.getId();
      if (blockMap.containsKey(tx.getBlockId())) {
        tx.setBlock(blockMap.get(tx.getBlockId()));
      }
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

  private List<TxFilterResponse> mapDataFromTxByAddressListToResponses(
      Page<Tx> txPage, Address address) {
    List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage);

    Set<Long> txIdList = txPage.getContent().stream().map(Tx::getId).collect(Collectors.toSet());

    // get address tx balance
    List<AddressTxBalance> addressTxBalances =
        addressTxBalanceRepository.findByTxIdInAndByAddress(txIdList, address.getAddress());
    Map<Long, List<AddressTxBalance>> addressTxBalanceMap =
        addressTxBalances.stream().collect(Collectors.groupingBy(AddressTxBalance::getTxId));

    // get address token
    List<AddressToken> addressTokens =
        addressTokenRepository.findByTxIdInAndByAddress(txIdList, address.getAddress());
    Map<Long, List<AddressToken>> addressTokenMap =
        addressTokens.stream()
            .filter(addressToken -> !BigInteger.ZERO.equals(addressToken.getBalance()))
            .collect(Collectors.groupingBy(addressToken -> addressToken.getTx().getId()));

    // get metadata and multi asset
    Pair<Map<String, AssetMetadata>, Map<Long, MultiAsset>> getMapMetadataAndMapAsset =
        getMapMetadataAndMapAsset(addressTokens);
    Map<Long, Address> addressMap = new HashMap<>() {{
      put(address.getId(), address);
    }};

    txFilterResponses.forEach(
        tx ->
            setAdditionalData(
                addressTxBalanceMap,
                addressTokenMap,
                getMapMetadataAndMapAsset.getFirst(),
                getMapMetadataAndMapAsset.getSecond(),
                addressMap,
                tx));
    return txFilterResponses;
  }

  private List<TxFilterResponse> mapDataFromTxProjectionListToResponseList(Page<TxProjection> txPage) {
    if (CollectionUtils.isEmpty(txPage.getContent())) {
      return new ArrayList<>();
    }
    Set<Long> blockIdList = txPage.getContent().stream().map(TxProjection::getBlockId)
        .collect(Collectors.toSet());
    List<Block> blocks = blockRepository.findAllByIdIn(blockIdList);
    Map<Long, Block> blockMap = blocks.stream()
        .collect(Collectors.toMap(Block::getId, Function.identity()));
    //get addresses input
    Set<Long> txIdSet = txPage.getContent().stream().map(TxProjection::getId).collect(Collectors.toSet());
    List<AddressInputOutputProjection> txInList = txOutRepository.findAddressInputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    //get addresses output
    List<AddressInputOutputProjection> txOutList = txOutRepository.findAddressOutputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressOutMap = txOutList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (TxProjection tx : txPage.getContent()) {
      Long txId = tx.getId();
      TxFilterResponse txResponse = txMapper.txProjectionToTxFilterResponse(tx);
      if (blockMap.containsKey(tx.getBlockId())) {
        Block block = blockMap.get(tx.getBlockId());
        txResponse.setEpochNo(block.getEpochNo());
        txResponse.setEpochSlotNo(block.getEpochSlotNo());
        txResponse.setSlot(block.getSlotNo().intValue());
        txResponse.setBlockNo(block.getBlockNo());
        txResponse.setBlockHash(block.getHash());
        txResponse.setTime(block.getTime().toLocalDateTime());
      }

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

  private List<TxFilterResponse> mapDataFromTxByStakeListToResponseList(Page<TxProjection> txPage) {
    List<TxFilterResponse> txFilterResponses = mapDataFromTxProjectionListToResponseList(txPage);
    Set<Long> txIdList =
        txPage.getContent().stream().map(TxProjection::getId).collect(Collectors.toSet());

    Set<Long> addressIdList =
        txPage.getContent().stream().map(TxProjection::getAddressId).collect(Collectors.toSet());

    // get address tx balance
    List<AddressTxBalance> addressTxBalances =
        addressTxBalanceRepository.findByTxIdInAndByAddressIdIn(txIdList, addressIdList);

    Map<Long, List<AddressTxBalance>> addressTxBalanceMap =
        addressTxBalances.stream().collect(Collectors.groupingBy(AddressTxBalance::getTxId));

    // get address token
    List<AddressToken> addressTokens =
        addressTokenRepository.findByTxIdInAndByAddressIn(txIdList, addressIdList);
    Map<Long, List<AddressToken>> addressTokenMap =
        addressTokens.stream()
            .filter(addressToken -> !BigInteger.ZERO.equals(addressToken.getBalance()))
            .collect(Collectors.groupingBy(addressToken -> addressToken.getTx().getId()));

    // get metadata and multi asset
    Pair<Map<String, AssetMetadata>, Map<Long, MultiAsset>> getMapMetadataAndMapAsset =
        getMapMetadataAndMapAsset(addressTokens);

    // get address
    List<Address> addresses = addressRepository.findAddressByIdIn(addressIdList);
    Map<Long, Address> addressMap =
        addresses.stream().collect(Collectors.toMap(Address::getId, Function.identity()));

    txFilterResponses.forEach(
        tx ->
            setAdditionalData(
                addressTxBalanceMap,
                addressTokenMap,
                getMapMetadataAndMapAsset.getFirst(),
                getMapMetadataAndMapAsset.getSecond(),
                addressMap,
                tx));
    return txFilterResponses;
  }

  private Pair<Map<String, AssetMetadata>, Map<Long, MultiAsset>> getMapMetadataAndMapAsset(
      List<AddressToken> addressTokens) {
    List<Long> multiAssetIdList =
        addressTokens.stream().map(AddressToken::getMultiAssetId).toList();
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

  private void setAdditionalData(
      Map<Long, List<AddressTxBalance>> addressTxBalanceMap,
      Map<Long, List<AddressToken>> addressTokenMap,
      Map<String, AssetMetadata> assetMetadataMap,
      Map<Long, MultiAsset> multiAssetMap,
      Map<Long, Address> addressMap,
      TxFilterResponse tx) {

    if (addressTxBalanceMap.containsKey(tx.getId())) {
      BigInteger balance =
          addressTxBalanceMap.get(tx.getId()).stream()
              .map(AddressTxBalance::getBalance)
              .reduce(BigInteger.ZERO, BigInteger::add);
      tx.setBalance(balance);
    }

    if (addressTokenMap.containsKey(tx.getId())) {
      List<TokenAddressResponse> tokenAddressResponses =
          addressTokenMap.get(tx.getId()).stream()
              .map(
                  addressToken -> {
                    MultiAsset multiAsset = multiAssetMap.get(addressToken.getMultiAssetId());
                    TokenAddressResponse taResponse = new TokenAddressResponse();
                    if (!Objects.isNull(multiAsset)) {
                      taResponse =
                          tokenMapper.fromMultiAssetAndAddressToken(multiAsset, addressToken);
                      String subject = multiAsset.getPolicy() + multiAsset.getName();
                      taResponse.setMetadata(
                          assetMetadataMapper.fromAssetMetadata(assetMetadataMap.get(subject)));
                    }

                    Address address = addressMap.get(addressToken.getAddressId());
                    if (!Objects.isNull(address)) {
                      taResponse.setAddress(address.getAddress());
                    }

                    return taResponse;
                  })
              .toList();
      tx.setTokens(tokenAddressResponses);
    }
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

    if (Objects.nonNull(txResponse.getTx().getEpochNo())) {
      Epoch epoch = epochRepository.findFirstByNo(txResponse.getTx().getEpochNo()).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      txResponse.getTx().setMaxEpochSlot(epoch.getMaxSlot());
    }
    if (Objects.nonNull(txResponse.getTx().getBlockNo())) {
      txResponse.getTx().setConfirmation(currentBlockNo - txResponse.getTx().getBlockNo());
    } else {
      txResponse.getTx().setConfirmation(currentBlockNo);
    }

    // get address input output
    getSummaryAndUTxOs(tx, txResponse);
    getContracts(tx, txResponse);
    getCollaterals(tx, txResponse);
    getWithdrawals(tx, txResponse);
    getDelegations(tx, txResponse);
    getMints(tx, txResponse);
    getStakeCertificates(tx, txResponse);
    getPoolCertificates(tx, txResponse);
    getProtocols(tx, txResponse);
    /*
     * If the transaction is invalid, the collateral is the input and the output of the transaction.
     * Otherwise, the collateral is the input and the output of the collateral.
     */
    if (Boolean.TRUE.equals(tx.getValidContract())) {
      txResponse.getTx().setStatus(TxStatus.SUCCESS);
    } else {
      txResponse.getTx().setStatus(TxStatus.FAIL);
      CollateralResponse collateralResponse = txResponse.getCollaterals();
      List<TxOutResponse> collateralInputs = collateralResponse.getCollateralInputResponses();
      List<TxOutResponse> collateralOutputs = collateralResponse.getCollateralOutputResponses();
      collateralResponse.setCollateralInputResponses(txResponse.getUTxOs().getInputs());
      collateralResponse.setCollateralOutputResponses(txResponse.getUTxOs().getOutputs());
      txResponse.setCollaterals(collateralResponse);
      UTxOResponse uTxOResponse = new UTxOResponse();
      uTxOResponse.setInputs(collateralInputs);
      uTxOResponse.setOutputs(collateralOutputs);
      txResponse.setUTxOs(uTxOResponse);
    }

    return txResponse;
  }

  /**
   * Get transaction mints info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getMints(Tx tx, TxResponse txResponse) {
    List<MaTxMint> maTxMints = maTxMintRepository.findByTx(tx);
    Set<String> subjects = maTxMints.stream().map(
        ma -> ma.getIdent().getPolicy() + ma.getIdent().getName()).collect(Collectors.toSet());
    var assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    var assetMetadataMap = assetMetadataList.stream().collect(Collectors.toMap(
        AssetMetadata::getSubject, Function.identity()
    ));
    if (!CollectionUtils.isEmpty(maTxMints)) {
      txResponse.setMints(maTxMints.stream().map(
          ma -> {
            TxMintingResponse txMintingResponse = maTxMintMapper.fromMaTxMint(ma);
            String subject = ma.getIdent().getPolicy() + ma.getIdent().getName();
            txMintingResponse.setMetadata(
                assetMetadataMapper.fromAssetMetadata(assetMetadataMap.get(subject)));
            return txMintingResponse;
          }).collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction delegations info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getDelegations(Tx tx, TxResponse txResponse) {
    List<Delegation> delegations = delegationRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(delegations)) {
      txResponse.setDelegations(delegations.stream().map(delegationMapper::fromDelegation)
          .collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction withdrawals info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getWithdrawals(Tx tx, TxResponse txResponse) {
    List<Withdrawal> withdrawals = withdrawalRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(withdrawals)) {
      List<String> addressToList = txResponse.getUTxOs().getOutputs().stream()
          .map(TxOutResponse::getAddress).collect(Collectors.toList());
      List<WithdrawalResponse> withdrawalResponses = withdrawals.stream().map(withdrawal -> {
        WithdrawalResponse withdrawalResponse = withdrawalMapper.fromWithdrawal(withdrawal);
        withdrawalResponse.setAddressTo(addressToList);
        return withdrawalResponse;
      }).collect(Collectors.toList());

      txResponse.setWithdrawals(withdrawalResponses);
    }
  }

  /**
   * Get transaction collaterals info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getCollaterals(Tx tx, TxResponse txResponse) {
    CollateralResponse collateralResponse = new CollateralResponse();
    List<AddressInputOutputProjection> collateralInputs = unconsumeTxInRepository
        .findTxCollateralInput(tx);
    List<AddressInputOutputProjection> collateralOutputs = failedTxOutRepository
        .findFailedTxOutByTx(tx);
    if (!CollectionUtils.isEmpty(collateralInputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap = collateralInputs
          .stream().collect(Collectors.groupingBy(
              txOutMapper::fromAddressInputOutput
          ));
      List<TxOutResponse> collateralInputResponse = mappingProjectionToAddress(addressInputMap);
      collateralResponse.setCollateralInputResponses(collateralInputResponse);
    }
    if (!CollectionUtils.isEmpty(collateralOutputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap = collateralOutputs
          .stream().collect(Collectors.groupingBy(
              txOutMapper::fromAddressInputOutput
          ));
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
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getContracts(Tx tx, TxResponse txResponse) {
    List<TxContractProjection> redeemers = redeemerRepository.findContractByTx(tx);
    if (!CollectionUtils.isEmpty(redeemers)) {
      List<ContractResponse> contractResponses = redeemers.stream().map(redeemer -> {
        if (redeemer.getPurpose().equals(ScriptPurposeType.SPEND)) {
          return new ContractResponse(redeemer.getAddress());
        } else {
          return new ContractResponse(redeemer.getScriptHash());
        }
      }).collect(Collectors.toList());
      txResponse.setContracts(contractResponses);
    }
  }

  /**
   * Get transaction summary and UTxOs info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getSummaryAndUTxOs(Tx tx, TxResponse txResponse) {
    List<AddressInputOutputProjection> addressInputInfo = txOutRepository.getTxAddressInputInfo(
        tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap = addressInputInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromAddressInputOutput
        ));
    List<AddressInputOutputProjection> addressOutputInfo = txOutRepository.getTxAddressOutputInfo(
        tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap = addressOutputInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromAddressInputOutput
        ));
    List<TxOutResponse> uTxOOutputs = mappingProjectionToAddress(addressOutputMap);
    List<TxOutResponse> uTxOInputs = mappingProjectionToAddress(addressInputMap);
    UTxOResponse uTxOs = UTxOResponse.builder()
        .inputs(uTxOInputs)
        .outputs(uTxOOutputs)
        .build();
    txResponse.setUTxOs(uTxOs);

    List<TxOutResponse> addressesInfoInput = getStakeAddressInfo(addressInputInfo);
    List<TxOutResponse> addressesInfoOutput = getStakeAddressInfo(addressOutputInfo);

    List<TxOutResponse> stakeAddress = removeDuplicateTx(addressesInfoInput, addressesInfoOutput);

    SummaryResponse summary =
        SummaryResponse.builder()
            .stakeAddress(stakeAddress)
            .build();

    txResponse.setSummary(summary);

  }

  private List<TxOutResponse> removeDuplicateTx(
      List<TxOutResponse> addressesInputs, List<TxOutResponse> addressesOutputs) {
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
          if (txs.size() > 1) {
            BigInteger totalValue = txs.get(0).getValue().add(txs.get(1).getValue());
            txs.get(0).setValue(totalValue);

            Map<String, List<TxMintingResponse>> unionTokenByAsset =
                Stream.concat(txs.get(0).getTokens().stream(), txs.get(1).getTokens().stream())
                    .collect(
                        Collectors.groupingBy(
                            TxMintingResponse::getAssetId,
                            Collectors.toCollection(ArrayList::new)));

            List<TxMintingResponse> tokenResponse = new ArrayList<>();

            unionTokenByAsset.forEach(
                (asset, tokens) -> {
                  BigInteger totalQuantity =
                      tokens.stream()
                          .map(TxMintingResponse::getAssetQuantity)
                          .reduce(BigInteger.ZERO, BigInteger::add);

                  if (!BigInteger.ZERO.equals(totalQuantity)) {
                    tokens.get(0).setAssetQuantity(totalQuantity);
                    tokenResponse.add(tokens.get(0));
                  }
                });

            txs.get(0).setTokens(tokenResponse);
          }

          summary.add(txs.get(0));
        });

    return summary.stream()
        .sorted(Comparator.comparing(TxOutResponse::getValue))
        .collect(Collectors.toList());
  }

  /**
   * Get transaction stake certificates info
   *
   * @param tx         transaction
   * @param txResponse response data of stake certificates
   */
  private void getStakeCertificates(Tx tx, TxResponse txResponse) {
    List<TxStakeCertificate> stakeCertificates = stakeRegistrationRepository
        .findByTx(tx).stream().map(
            item -> new TxStakeCertificate(item.getAddr().getView(),
                CertificateType.STAKE_REGISTRATION)
        ).collect(Collectors.toList());
    stakeCertificates.addAll(stakeDeRegistrationRepository.findByTx(tx).stream()
        .map(
            item -> new TxStakeCertificate(item.getAddr().getView(),
                CertificateType.STAKE_DEREGISTRATION)
        ).collect(Collectors.toList()));
    if (!CollectionUtils.isEmpty(stakeCertificates)) {
      txResponse.setStakeCertificates(stakeCertificates);
    }
  }

  /**
   * Get transaction pool certificates info
   *
   * @param tx         transaction
   * @param txResponse response data of pool certificates
   */
  private void getPoolCertificates(Tx tx, TxResponse txResponse) {

    // get pool registration
    List<PoolUpdateDetailProjection> poolUpdateDetailProjections = poolUpdateRepository
        .findByTx(tx);
    Set<Long> poolUpdateIdSet = poolUpdateDetailProjections.stream().map(
        PoolUpdateDetailProjection::getPoolUpdateId).collect(Collectors.toSet());
    List<StakeKeyProjection> poolOwners = poolUpdateRepository.findOwnerAccountByPoolUpdate(
        poolUpdateIdSet);
    Map<Long, List<String>> poolOwnerMap = poolOwners.stream().collect(Collectors.groupingBy(
        StakeKeyProjection::getPoolUpdateId,
        Collectors.mapping(StakeKeyProjection::getView, Collectors.toList())
    ));
    List<PoolRelayProjection> poolRelays = poolRelayRepository.findByPoolHashIdIn(poolUpdateIdSet);
    Map<Long, List<PoolRelayProjection>> poolRelayMap =
        poolRelays.stream().collect(Collectors.groupingBy(PoolRelayProjection::getPoolUpdateId));
    List<TxPoolCertificate> poolCertificates = poolUpdateDetailProjections.stream()
        .map(
            item -> {
              List<PoolRelayResponse> relays = null;
              if (poolRelayMap.containsKey(item.getPoolUpdateId())) {
                relays = poolRelayMap.get(item.getPoolUpdateId())
                    .stream().map(relay ->
                        PoolRelayResponse.builder()
                            .dnsName(relay.getDnsName())
                            .dnsSrvName(relay.getDnsSrvName())
                            .ipv4(relay.getIpv4())
                            .ipv6(relay.getIpv6())
                            .port(relay.getPort())
                            .build()).collect(Collectors.toList());
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
            }
        ).collect(Collectors.toList());

    //get pool de registration
    List<PoolDeRegistrationProjection> poolRetireDetailProjections
        = poolRetireRepository.findByAnnouncedTx(tx);
    poolCertificates.addAll(poolRetireDetailProjections.stream().map(
        item -> TxPoolCertificate.builder()
            .poolId(item.getPoolId())
            .epoch(item.getRetiringEpoch())
            .type(CertificateType.POOL_DEREGISTRATION)
            .build()
    ).collect(Collectors.toList()));
    if (!CollectionUtils.isEmpty(poolCertificates)) {
      txResponse.setPoolCertificates(poolCertificates);
    }
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
      List<TxMintingResponse> tokens = addressInputOutputMap.get(uTxO).stream().
          filter(token -> Objects.nonNull(token.getAssetId())).map(
              maTxMintMapper::fromAddressInputOutputProjection
          ).collect(Collectors.toList());
      tokens.addAll(getTokenInFailedTxOut(addressInputOutputMap, uTxO));
      uTxO.setTokens(tokens);
    }
    return uTxOs;
  }

  /**
   * If data from  collateral output, handle token in json string data Otherwise, not exist json
   * string data Example of json string data:
   * [{"unit":"LOVELACE","policyId":"","assetName":"TE9WRUxBQ0U=","quantity":2726335}]
   *
   * @param addressInputOutputMap address projection map
   * @param txOut                 list collateral output
   */
  private List<TxMintingResponse> getTokenInFailedTxOut(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap,
      TxOutResponse txOut) {
    List<TxMintingResponse> tokens = new ArrayList<>();
    List<String> tokenStringList = addressInputOutputMap.get(txOut).stream().map(
        AddressInputOutputProjection::getAssetsJson).filter(
        StringUtils::isNotEmpty
    ).toList();
    ObjectMapper objectMapper = new ObjectMapper();
    tokenStringList.forEach(tokenString -> {
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
                String assetDisplayName = HexUtils.fromHex(assetName,
                    AssetUtil.calculateFingerPrint(token.get("policyId").asText(), assetName));
                TxMintingResponse txMintingResponse = TxMintingResponse.builder()
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
        }
    );
    return tokens;
  }

  /**
   * Get stake address info from address
   *
   * @param addressInfo List address input or output info
   * @return list stake address input or output info
   */
  private List<TxOutResponse> getStakeAddressInfo(
      List<AddressInputOutputProjection> addressInfo) {

    Map<TxOutResponse, List<AddressInputOutputProjection>> addressMap = addressInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromStakeAddressInputOutput
        ));
    Map<String, List<AddressInputOutputProjection>> addressTokenMap = addressInfo.stream()
        .collect(Collectors.groupingBy(
            AddressInputOutputProjection::getStakeAddress
        ));
    var addressValueMap = addressMap.keySet().stream().collect(Collectors.groupingBy(
        TxOutResponse::getAddress,
        Collectors.reducing(BigInteger.ZERO, TxOutResponse::getValue,
            BigInteger::add)
    ));
    List<TxOutResponse> stakeAddressTxInputList = new ArrayList<>();
    addressValueMap.forEach(
        (key, value) -> stakeAddressTxInputList.add(TxOutResponse.builder()
            .address(key)
            .value(value)
            .tokens(addressTokenMap.get(key).stream()
                .filter(token -> Objects.nonNull(token.getAssetId()))
                .map(maTxMintMapper::fromAddressInputOutputProjection).collect(Collectors.toList()))
            .build())
    );
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

    final LocalDateTime previousMonth = currentLocalDate.minusMonths(BigInteger.ONE.longValue());

    final BigInteger previousMonthInSeconds = BigInteger
        .valueOf(previousMonth.toInstant(ZoneOffset.UTC).getEpochSecond());

    long maxDay = ChronoUnit.DAYS.between(previousMonth, currentLocalDate);

    if (maxDay < day) {
      day = maxDay;
    }

    var keySize = redisTemplate.opsForList().size(key);
    // if key not exists or empty
    if (Objects.isNull(keySize) || keySize.equals(BigInteger.ZERO.longValue())) {
      List<TxGraph> txCharts = toTxGraph(txChartRepository
          .getTransactionGraphDayGreaterThan(previousMonthInSeconds));
      updateRedisTxGraph(txCharts, key, Boolean.TRUE);

      return getTxGraphsByDayRange((int) day, txCharts.stream()
          .sorted(Comparator.comparing(TxGraph::getDate)
              .reversed()).toList());
    }

    // txGraphsRedis will not empty
    List<TxGraph> txGraphsRedis = redisTemplate.opsForList()
        .range(key, BigInteger.ZERO.longValue(), DAYS_IN_MONTH);

    final var latestDay = txGraphsRedis.get(BigInteger.ZERO.intValue()).getDate();
    final var latestLocalDateTime = Instant.ofEpochMilli(latestDay.getTime())
        .atZone(ZoneOffset.UTC)
        .toLocalDateTime();

    while (ChronoUnit.DAYS.between(
        LocalDateTime.ofInstant(txGraphsRedis.get(txGraphsRedis.size() - 1).getDate().toInstant(),
            ZoneOffset.UTC), currentLocalDate) > DAYS_IN_MONTH) {
      redisTemplate.opsForList().rightPop(key);
      txGraphsRedis.remove(txGraphsRedis.size() - BigInteger.ONE.intValue());
    }

    var distanceFromRealAndCache = ChronoUnit.DAYS.between(latestLocalDateTime, currentLocalDate);

    if (distanceFromRealAndCache >= BigInteger.TWO.longValue()) {
      final long dayMissingData = day - distanceFromRealAndCache;
      if (dayMissingData <= BigInteger.ZERO.longValue()) {
        return Collections.emptyList();
      }

      List<TxGraph> txCharts = toTxGraph(txChartRepository
          .getTransactionGraphDayGreaterThan(previousMonthInSeconds));
      updateRedisTxGraph(txCharts, key, Boolean.TRUE);
      return getTxGraphsByDayRange((int) (day - distanceFromRealAndCache),
          txCharts.stream()
              .sorted(Comparator.comparing(TxGraph::getDate).reversed())
              .toList());
    }
    // get 2 days: today and yesterday
    List<BigInteger> days = LongStream.range(BigInteger.ZERO.intValue(), BigInteger.TWO.longValue())
        .boxed()
        .map(dayMinus -> {
          var markDay = currentLocalDate.minusDays(dayMinus);
          return BigInteger.valueOf(markDay.toInstant(ZoneOffset.UTC).getEpochSecond());
        }).toList();

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
      redisTemplate.opsForList()
          .set(key, BigInteger.ZERO.intValue(), previousChart);
      redisTemplate.opsForList().leftPush(key, latestChart);

      txGraphs.add(latestChart);
      txGraphs.add(previousChart);
      txGraphs.addAll(txGraphsRedis.subList(BigInteger.ONE.intValue(),
          txGraphsRedis.size()));

      return getTxGraphsByDayRange((int) day, txGraphs);
    }

    txGraphs.add(latestChart);
    txGraphs.add(previousChart);
    txGraphs.addAll(txGraphsRedis.subList(BigInteger.TWO.intValue(),
        txGraphsRedis.size()));

    return getTxGraphsByDayRange((int) day, txGraphs);
  }

  private static List<TxGraph> getTxGraphsByDayRange(int day, List<TxGraph> txCharts) {

    if (txCharts.size() < day) {
      return txCharts
          .stream()
          .sorted(Comparator.comparing(TxGraph::getDate))
          .toList();
    }

    return txCharts
        .subList(BigInteger.ZERO.intValue(), day)
        .stream()
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

    List<BigInteger> hours = LongStream.range(BigInteger.ZERO.intValue(), HOURS_IN_DAY)
        .boxed()
        .map(currentLoaLocalDateTime::minusHours)
        .map(hour -> BigInteger.valueOf(hour.toInstant(ZoneOffset.UTC).getEpochSecond()))
        .toList();

    List<TxGraphProjection> txs = txChartRepository.getTransactionGraphByHour(hours);

    if (CollectionUtils.isEmpty(txs)) {
      return Collections.emptyList();
    }

    return toTxGraph(txs).stream()
        .sorted(Comparator.comparing(TxGraph::getDate))
        .toList();
  }

  /**
   * Mapping projection to TxGraph
   *
   * @param txs
   * @return
   */
  private static List<TxGraph> toTxGraph(List<TxGraphProjection> txs) {
    return txs.stream()
        .map(txChart -> TxGraph.builder()
            .date(Date.from(
                OffsetDateTime.ofInstant(Instant.ofEpochSecond(txChart.getTime().longValue()),
                    ZoneOffset.UTC).toInstant()))
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
      txGraphs.forEach(txGraph ->
          redisTemplate.opsForList().leftPush(key, txGraph));
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
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getProtocols(Tx tx, TxResponse txResponse) {

    List<ParamProposal> paramProposals = paramProposalRepository
        .getParamProposalByRegisteredTxId(tx.getId());

    if (!ObjectUtils.isEmpty(paramProposals)) {
      //find current change param
      txResponse.setProtocols(protocolMapper.mapProtocolParamResponse(paramProposals));
      //get previous value
      if (Objects.nonNull(txResponse.getProtocols())) {
        Integer epochNo = tx.getBlock().getEpochNo();

        epochParamRepository.findEpochParamByEpochNo(epochNo)
            .ifPresent(epochParam ->
                txResponse.setPreviousProtocols(
                    protocolMapper.mapPreviousProtocolParamResponse(epochParam,
                        txResponse.getProtocols()))
            );
      }
      processCheckIfSameProtocol(txResponse);
    }
  }

  private void processCheckIfSameProtocol(TxResponse txResponse) {
    ProtocolParamResponse currentProtocol = txResponse.getProtocols();
    ProtocolParamResponse previousProtocol = txResponse.getPreviousProtocols();

    if (Objects.nonNull(currentProtocol.getMinFeeA())
        && Objects.hashCode(currentProtocol.getMinFeeA()) == Objects.hashCode(
        previousProtocol.getMinFeeA())
    ) {
      currentProtocol.setMinFeeA(null);
      previousProtocol.setMinFeeA(null);
    }

    if (Objects.nonNull(currentProtocol.getMinFeeB())
        && Objects.hashCode(currentProtocol.getMinFeeB()) == Objects.hashCode(
        previousProtocol.getMinFeeB())
    ) {
      currentProtocol.setMinFeeB(null);
      previousProtocol.setMinFeeB(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockSize())
        && Objects.hashCode(currentProtocol.getMaxBlockSize()) == Objects.hashCode(
        previousProtocol.getMaxBlockSize())
    ) {
      currentProtocol.setMaxBlockSize(null);
      previousProtocol.setMaxBlockSize(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxSize())
        && Objects.hashCode(currentProtocol.getMaxTxSize()) == Objects.hashCode(
        previousProtocol.getMaxTxSize())
    ) {
      currentProtocol.setMaxTxSize(null);
      previousProtocol.setMaxTxSize(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBhSize())
        && Objects.hashCode(currentProtocol.getMaxBhSize()) == Objects.hashCode(
        previousProtocol.getMaxBhSize())
    ) {
      currentProtocol.setMaxBhSize(null);
      previousProtocol.setMaxBhSize(null);
    }

    if (Objects.nonNull(currentProtocol.getKeyDeposit())
        && Objects.hashCode(currentProtocol.getKeyDeposit()) == Objects.hashCode(
        previousProtocol.getKeyDeposit())
    ) {
      currentProtocol.setKeyDeposit(null);
      previousProtocol.setKeyDeposit(null);
    }

    if (Objects.nonNull(currentProtocol.getPoolDeposit())
        && Objects.hashCode(currentProtocol.getPoolDeposit()) == Objects.hashCode(
        previousProtocol.getPoolDeposit())
    ) {
      currentProtocol.setPoolDeposit(null);
      previousProtocol.setPoolDeposit(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxEpoch())
        && Objects.hashCode(currentProtocol.getMaxEpoch()) == Objects.hashCode(
        previousProtocol.getMaxEpoch())
    ) {
      currentProtocol.setMaxEpoch(null);
      previousProtocol.setMaxEpoch(null);
    }

    if (Objects.nonNull(currentProtocol.getOptimalPoolCount())
        && Objects.hashCode(currentProtocol.getOptimalPoolCount()) == Objects.hashCode(
        previousProtocol.getOptimalPoolCount())
    ) {
      currentProtocol.setOptimalPoolCount(null);
      previousProtocol.setOptimalPoolCount(null);
    }

    if (Objects.nonNull(currentProtocol.getMinUtxoValue())
        && Objects.hashCode(currentProtocol.getMinUtxoValue()) == Objects.hashCode(
        previousProtocol.getMinUtxoValue())
    ) {
      currentProtocol.setMinUtxoValue(null);
      previousProtocol.setMinUtxoValue(null);
    }

    if (Objects.nonNull(currentProtocol.getMinPoolCost())
        && Objects.hashCode(currentProtocol.getMinPoolCost()) == Objects.hashCode(
        previousProtocol.getMinPoolCost())
    ) {
      currentProtocol.setMinPoolCost(null);
      previousProtocol.setMinPoolCost(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxExMem())
        && Objects.hashCode(currentProtocol.getMaxTxExMem()) == Objects.hashCode(
        previousProtocol.getMaxTxExMem())
    ) {
      currentProtocol.setMaxTxExMem(null);
      previousProtocol.setMaxTxExMem(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxTxExSteps())
        && Objects.hashCode(currentProtocol.getMaxTxExSteps()) == Objects.hashCode(
        previousProtocol.getMaxTxExSteps())
    ) {
      currentProtocol.setMaxTxExSteps(null);
      previousProtocol.setMaxTxExSteps(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockExMem())
        && Objects.hashCode(currentProtocol.getMaxBlockExMem()) == Objects.hashCode(
        previousProtocol.getMaxBlockExMem())
    ) {
      currentProtocol.setMaxBlockExMem(null);
      previousProtocol.setMaxBlockExMem(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxBlockExSteps())
        && Objects.hashCode(currentProtocol.getMaxBlockExSteps()) == Objects.hashCode(
        previousProtocol.getMaxBlockExSteps())
    ) {
      currentProtocol.setMaxBlockExSteps(null);
      previousProtocol.setMaxBlockExSteps(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxValSize())
        && Objects.hashCode(currentProtocol.getMaxValSize()) == Objects.hashCode(
        previousProtocol.getMaxValSize())
    ) {
      currentProtocol.setMaxValSize(null);
      previousProtocol.setMaxValSize(null);
    }

    if (Objects.nonNull(currentProtocol.getCoinsPerUtxoSize())
        && Objects.hashCode(currentProtocol.getCoinsPerUtxoSize()) == Objects.hashCode(
        previousProtocol.getCoinsPerUtxoSize())
    ) {
      currentProtocol.setCoinsPerUtxoSize(null);
      previousProtocol.setCoinsPerUtxoSize(null);
    }

    if (Objects.nonNull(currentProtocol.getInfluence())
        && Objects.hashCode(currentProtocol.getInfluence()) == Objects.hashCode(
        previousProtocol.getInfluence())
    ) {
      currentProtocol.setInfluence(null);
      previousProtocol.setInfluence(null);
    }

    if (Objects.nonNull(currentProtocol.getMonetaryExpandRate())
        && Objects.hashCode(currentProtocol.getMonetaryExpandRate()) == Objects.hashCode(
        previousProtocol.getMonetaryExpandRate())
    ) {
      currentProtocol.setMonetaryExpandRate(null);
      previousProtocol.setMonetaryExpandRate(null);
    }

    if (Objects.nonNull(currentProtocol.getTreasuryGrowthRate())
        && Objects.hashCode(currentProtocol.getTreasuryGrowthRate()) == Objects.hashCode(
        previousProtocol.getTreasuryGrowthRate())
    ) {
      currentProtocol.setTreasuryGrowthRate(null);
      previousProtocol.setTreasuryGrowthRate(null);
    }

    if (Objects.nonNull(currentProtocol.getDecentralisation())
        && Objects.hashCode(currentProtocol.getDecentralisation()) == Objects.hashCode(
        previousProtocol.getDecentralisation())
    ) {
      currentProtocol.setDecentralisation(null);
      previousProtocol.setDecentralisation(null);
    }

    if (Objects.nonNull(currentProtocol.getPriceMem())
        && Objects.hashCode(currentProtocol.getPriceMem()) == Objects.hashCode(
        previousProtocol.getPriceMem())
    ) {
      currentProtocol.setPriceMem(null);
      previousProtocol.setPriceMem(null);
    }

    if (Objects.nonNull(currentProtocol.getPriceStep())
        && Objects.hashCode(currentProtocol.getPriceStep()) == Objects.hashCode(
        previousProtocol.getPriceStep())
    ) {
      currentProtocol.setPriceStep(null);
      previousProtocol.setPriceStep(null);
    }

    if (Objects.nonNull(currentProtocol.getProtocolMajor())
        && Objects.hashCode(currentProtocol.getProtocolMajor()) == Objects.hashCode(
        previousProtocol.getProtocolMajor())
        && Objects.nonNull(currentProtocol.getProtocolMinor())
        && Objects.hashCode(currentProtocol.getProtocolMinor()) == Objects.hashCode(
        previousProtocol.getProtocolMinor())
    ) {
      currentProtocol.setProtocolMajor(null);
      previousProtocol.setProtocolMajor(null);
      currentProtocol.setProtocolMinor(null);
      previousProtocol.setProtocolMinor(null);
    }

    if (Objects.nonNull(currentProtocol.getCollateralPercent())
        && Objects.hashCode(currentProtocol.getCollateralPercent()) == Objects.hashCode(
        previousProtocol.getCollateralPercent())
    ) {
      currentProtocol.setCollateralPercent(null);
      previousProtocol.setCollateralPercent(null);
    }

    if (Objects.nonNull(currentProtocol.getMaxCollateralInputs())
        && Objects.hashCode(currentProtocol.getMaxCollateralInputs()) == Objects.hashCode(
        previousProtocol.getMaxCollateralInputs())
    ) {
      currentProtocol.setMaxCollateralInputs(null);
      previousProtocol.setMaxCollateralInputs(null);
    }

    if (Objects.nonNull(currentProtocol.getEntropy())
        && Objects.hashCode(currentProtocol.getEntropy()) == Objects.hashCode(
        previousProtocol.getEntropy())
    ) {
      currentProtocol.setEntropy(null);
      previousProtocol.setEntropy(null);
    }

    if (Objects.nonNull(currentProtocol.getCostModel())
        && Objects.hashCode(currentProtocol.getCostModel()) == Objects.hashCode(
        previousProtocol.getCostModel())
    ) {
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
