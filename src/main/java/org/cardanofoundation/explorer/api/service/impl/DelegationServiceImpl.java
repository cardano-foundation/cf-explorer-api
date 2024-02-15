package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonObject;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.address.DelegationPoolResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.DelegatorChartList;
import org.cardanofoundation.explorer.api.model.response.pool.chart.DelegatorChartResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.EpochChartList;
import org.cardanofoundation.explorer.api.model.response.pool.chart.EpochChartResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.BasePoolChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoKoiosProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.explorer.AggregatePoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.PoolCertificateService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.common.entity.explorer.AggregatePoolInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.BaseEntity_;
import org.cardanofoundation.explorer.common.entity.ledgersync.Delegation_;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolHash;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolOfflineData_;
import org.cardanofoundation.explorer.common.exception.BusinessException;
import org.cardanofoundation.explorer.common.exception.CommonErrorCode;

@Service
@RequiredArgsConstructor
@Log4j2
public class DelegationServiceImpl implements DelegationService {

  private final DelegationRepository delegationRepository;

  private final BlockRepository blockRepository;

  private final EpochRepository epochRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final PoolHashRepository poolHashRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  private final RewardRepository rewardRepository;

  private final PoolInfoRepository poolInfoRepository;

  private final PoolHistoryRepository poolHistoryRepository;

  private final RedisTemplate<String, Object> redisTemplate;

  private final FetchRewardDataService fetchRewardDataService;

  private final StakeAddressRepository stakeAddressRepository;

  private final WithdrawalRepository withdrawalRepository;

  private final TxRepository txRepository;

  private final EpochService epochService;

  private final AggregatePoolInfoRepository aggregatePoolInfoRepository;

  private final PoolCertificateService poolCertificateService;

  private static final int MAX_TOTAL_ELEMENTS = 1000;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;

  @Value("${application.network}")
  private String network;

  @Override
  public BaseFilterResponse<DelegationResponse> getDelegations(Pageable pageable) {
    Page<Long> txIdPage = delegationRepository.findAllDelegations(pageable);
    List<TxIOProjection> txs = txRepository.findTxIn(txIdPage.getContent());
    Map<Long, TxIOProjection> txMap =
        txs.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));
    List<DelegationProjection> delegations =
        delegationRepository.findDelegationByTxIdIn(txIdPage.getContent());
    Map<Long, List<String>> delegationStakeKeyMap =
        delegations.stream()
            .collect(
                Collectors.groupingBy(
                    DelegationProjection::getTxId,
                    Collectors.mapping(
                        DelegationProjection::getStakeAddress, Collectors.toList())));
    Map<Long, Set<DelegationPoolResponse>> delegationPoolMap =
        delegations.stream()
            .collect(
                Collectors.groupingBy(
                    DelegationProjection::getTxId,
                    Collectors.mapping(
                        item ->
                            DelegationPoolResponse.builder()
                                .poolName(item.getPoolName())
                                .tickerName(item.getTickerName())
                                .poolId(item.getPoolView())
                                .build(),
                        Collectors.toSet())));
    List<DelegationResponse> responses =
        txIdPage.stream()
            .map(
                item ->
                    DelegationResponse.builder()
                        .txHash(txMap.get(item).getHash())
                        .blockNo(txMap.get(item).getBlockNo())
                        .epochNo(txMap.get(item).getEpochNo())
                        .epochSlotNo(txMap.get(item).getEpochSlotNo())
                        .time(txMap.get(item).getTime())
                        .epochSlotNo(txMap.get(item).getEpochSlotNo())
                        .slotNo(txMap.get(item).getSlot())
                        .stakeKeys(delegationStakeKeyMap.get(item))
                        .pools(delegationPoolMap.get(item).stream().toList())
                        .build())
            .toList();
    return new BaseFilterResponse<>(txIdPage, responses);
  }

  @Override
  public DelegationHeaderResponse getDataForDelegationHeader() {
    EpochSummary epoch = epochService.getCurrentEpochSummary();
    Integer epochNo = epoch.getNo();
    Object poolActiveObj =
        redisTemplate.opsForValue().get(CommonConstant.REDIS_POOL_ACTIVATE + network);
    Object poolInActiveObj =
        redisTemplate.opsForValue().get(CommonConstant.REDIS_POOL_INACTIVATE + network);
    LocalDateTime endTime = epoch.getEndTime();
    Integer slot = epoch.getSlot();
    long countDownTime =
        Timestamp.valueOf(endTime).getTime()
            - Timestamp.valueOf(LocalDateTime.now(ZoneOffset.UTC)).getTime();
    BigInteger liveStake;
    boolean useKoiOs = fetchRewardDataService.useKoios();
    if (useKoiOs) {
      log.info("using koiOs flow...");
      if (Boolean.FALSE.equals(fetchRewardDataService.checkAdaPots(epochNo))) {
        fetchRewardDataService.fetchAdaPots(List.of(epochNo));
      }
      liveStake = poolInfoRepository.getTotalLiveStake(epochNo);
    } else {
      log.info("using base flow...");
      liveStake = null;
    }
    Object delegatorCached =
        redisTemplate.opsForValue().get(CommonConstant.REDIS_TOTAL_DELEGATOR + network);
    Integer delegators =
        Objects.nonNull(delegatorCached)
            ? Integer.parseInt(String.valueOf(delegatorCached))
            : CommonConstant.ZERO;
    return DelegationHeaderResponse.builder()
        .epochNo(epochNo)
        .epochSlotNo(slot)
        .liveStake(liveStake)
        .delegators(delegators)
        .activePools(Objects.nonNull(poolActiveObj) ? (Integer) poolActiveObj : CommonConstant.ZERO)
        .retiredPools(
            Objects.nonNull(poolInActiveObj) ? (Integer) poolInActiveObj : CommonConstant.ZERO)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(
      Pageable pageable, String search, boolean showRetired) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    Set<Long> poolRetiredIds = new HashSet<>();
    if (!showRetired) {
      String poolRetiredIdKey = CommonConstant.POOL_IDS_INACTIVATE + network;
      poolRetiredIds =
          redisTemplate.opsForHash().values(poolRetiredIdKey).stream()
              .map(item -> Long.parseLong(String.valueOf(item)))
              .collect(Collectors.toSet());
    }
    // add -1L to poolRetiredIds to avoid empty list
    poolRetiredIds.add(-1L);
    boolean isQueryEmpty = DataUtil.isNullOrEmpty(search);
    if (isQueryEmpty) {
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.ASC, BaseEntity_.ID));
    } else {
      search = search.toLowerCase();
      String poolNameLength = "poolNameLength";
      if (MAX_TOTAL_ELEMENTS / pageable.getPageSize() <= pageable.getPageNumber()) {
        throw new BusinessException(BusinessCode.OUT_OF_QUERY_LIMIT);
      }
      pageable =
          createPageableWithSort(
              pageable, Sort.by(Sort.Direction.ASC, poolNameLength, PoolOfflineData_.POOL_NAME));
    }
    Integer epochNo = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    Page<PoolResponse> poolResponse =
        getPoolResponseList(search, pageable, poolRetiredIds, epochNo);
    response.setData(poolResponse.getContent());
    response.setTotalItems(poolResponse.getTotalElements());
    response.setTotalPages(poolResponse.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    if (!isQueryEmpty) {
      response.setIsDataOverSize(poolResponse.getTotalElements() >= 1000);
    }
    return response;
  }

  private Page<PoolResponse> getPoolResponseList(
      String queryParam, Pageable pageable, Set<Long> retiredIds, int epochNo) {
    List<String> sortProperties = pageable.getSort().stream().map(Sort.Order::getProperty).toList();
    boolean isSortedOnAggTable =
        sortProperties.contains("numberDelegators")
            || sortProperties.contains("epochBlock")
            || sortProperties.contains("lifetimeBlock");
    boolean isQueryEmpty = DataUtil.isNullOrEmpty(queryParam);
    boolean isKoiOs = fetchRewardDataService.useKoios();
    Page<PoolListProjection> poolInfoProjections;
    List<PoolResponse> poolResponseList;
    if (!isSortedOnAggTable) {
      if (isQueryEmpty) {
        poolInfoProjections =
            isKoiOs
                ? poolHashRepository.findAllWithoutQueryParam(retiredIds, epochNo, pageable)
                : poolHashRepository.findAllWithoutQueryParam(retiredIds, pageable);
      } else {
        poolInfoProjections =
            isKoiOs
                ? poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(
                    queryParam, retiredIds, epochNo, pageable)
                : poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(
                    queryParam, retiredIds, pageable);
      }
      poolResponseList = mapAggPoolInfoToPoolResponse(retiredIds, poolInfoProjections.getContent());
    } else {
      if (isQueryEmpty) {
        poolInfoProjections =
            aggregatePoolInfoRepository.findAllByPoolIdNotIn(retiredIds, pageable);
        poolResponseList =
            mapPoolInfoForPoolResponse(retiredIds, epochNo, poolInfoProjections.getContent());
      } else {
        return getAllByQueryAndSortByAggField(pageable, queryParam, retiredIds, epochNo);
      }
    }

    return new PageImpl<>(poolResponseList, pageable, poolInfoProjections.getTotalElements());
  }

  private Page<PoolResponse> getAllByQueryAndSortByAggField(
      Pageable pageable, String queryParam, Set<Long> retiredIds, int epochNo) {
    List<String> sortProperties = pageable.getSort().stream().map(Sort.Order::getProperty).toList();
    List<Direction> sortDirections =
        pageable.getSort().stream().map(Sort.Order::getDirection).toList();
    boolean isKoiOs = fetchRewardDataService.useKoios();
    List<PoolListProjection> poolListProjections =
        isKoiOs
            ? poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(
                queryParam, retiredIds, epochNo)
            : poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(queryParam, retiredIds);

    List<PoolResponse> poolResponseList =
        mapAggPoolInfoToPoolResponse(retiredIds, poolListProjections);
    if (sortProperties.contains("numberDelegators")) {
      poolResponseList.sort(
          Comparator.comparing(PoolResponse::getNumberDelegators)
              .thenComparing(PoolResponse::getPoolId));
    } else if (sortProperties.contains("epochBlock")) {
      poolResponseList.sort(
          Comparator.comparing(PoolResponse::getEpochBlock).thenComparing(PoolResponse::getPoolId));
    } else if (sortProperties.contains("lifetimeBlock")) {
      poolResponseList.sort(
          Comparator.comparing(PoolResponse::getLifetimeBlock)
              .thenComparing(PoolResponse::getPoolId));
    }

    if (sortDirections.get(0).equals(Direction.DESC)) {
      Collections.reverse(poolResponseList);
    }

    if (poolResponseList.size() > MAX_TOTAL_ELEMENTS) {
      poolResponseList = poolResponseList.subList(0, MAX_TOTAL_ELEMENTS);
    }

    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), poolResponseList.size());
    if (start > poolResponseList.size()) {
      return Page.empty();
    }
    return new PageImpl<>(poolResponseList.subList(start, end), pageable, poolResponseList.size());
  }

  /**
   * Map aggregate pool info for pool response
   *
   * @param retiredIds set of retired pool ids
   * @param poolListProjections
   * @return
   */
  private List<PoolResponse> mapAggPoolInfoToPoolResponse(
      Set<Long> retiredIds, List<PoolListProjection> poolListProjections) {
    List<Long> poolIds = poolListProjections.stream().map(PoolListProjection::getPoolId).toList();
    Map<Long, AggregatePoolInfo> aggPoolInfoMap =
        aggregatePoolInfoRepository.getAllByPoolIdIn(poolIds).stream()
            .collect(Collectors.toMap(AggregatePoolInfo::getPoolId, Function.identity()));

    return poolListProjections.stream()
        .map(
            projection -> {
              AggregatePoolInfo aggPoolInfo = aggPoolInfoMap.get(projection.getPoolId());
              return PoolResponse.builder()
                  .poolId(projection.getPoolView())
                  .id(projection.getPoolId())
                  .poolName(projection.getPoolName())
                  .tickerName(projection.getTickerName())
                  .pledge(projection.getPledge())
                  .feeAmount(projection.getFee())
                  .feePercent(projection.getMargin())
                  .poolSize(
                      BigInteger.valueOf(-1).equals(projection.getPoolSize())
                          ? null
                          : projection.getPoolSize())
                  .saturation(
                      Double.valueOf(-1).equals(projection.getSaturation())
                          ? null
                          : projection.getSaturation())
                  .numberDelegators(aggPoolInfo.getDelegatorCount())
                  .lifetimeBlock(aggPoolInfo.getBlockLifeTime())
                  .epochBlock(aggPoolInfo.getBlockInEpoch())
                  .retired(retiredIds.contains(projection.getPoolId()))
                  .build();
            })
        .collect(Collectors.toList());
  }

  private List<PoolResponse> mapPoolInfoForPoolResponse(
      Set<Long> retiredIds, int epochNo, List<PoolListProjection> poolListProjections) {
    boolean isKoiOs = fetchRewardDataService.useKoios();
    List<PoolResponse> poolResponseList;
    List<Long> poolIds = poolListProjections.stream().map(PoolListProjection::getPoolId).toList();
    Map<Long, PoolListProjection> poolListProjectionMap =
        (isKoiOs
                ? poolHashRepository.findAllByPoolIdIn(poolIds, epochNo)
                : poolHashRepository.findAllByPoolIdIn(poolIds))
            .stream().collect(Collectors.toMap(PoolListProjection::getPoolId, Function.identity()));

    poolResponseList =
        poolListProjections.stream()
            .map(
                pool -> {
                  PoolListProjection poolListProjection =
                      poolListProjectionMap.get(pool.getPoolId());
                  return PoolResponse.builder()
                      .poolId(poolListProjection.getPoolView())
                      .id(poolListProjection.getPoolId())
                      .poolName(poolListProjection.getPoolName())
                      .tickerName(poolListProjection.getTickerName())
                      .pledge(poolListProjection.getPledge())
                      .feeAmount(poolListProjection.getFee())
                      .feePercent(poolListProjection.getMargin())
                      .poolSize(
                          BigInteger.valueOf(-1).equals(poolListProjection.getPoolSize())
                              ? null
                              : poolListProjection.getPoolSize())
                      .saturation(
                          Double.valueOf(-1).equals(poolListProjection.getSaturation())
                              ? null
                              : poolListProjection.getSaturation())
                      .numberDelegators(pool.getNumberDelegators())
                      .lifetimeBlock(pool.getLifetimeBlock())
                      .epochBlock(pool.getEpochBlock())
                      .retired(retiredIds.contains(pool.getPoolId()))
                      .build();
                })
            .collect(Collectors.toList());
    return poolResponseList;
  }

  /**
   * Create pageable with sort, if sort is unsorted then use default sort
   *
   * @param pageable page information
   * @param defaultSort default sort condition
   * @return pageable with sort
   */
  private Pageable createPageableWithSort(Pageable pageable, Sort defaultSort) {
    Sort sort = pageable.getSort();
    if (sort.isUnsorted()) {
      sort = defaultSort;
    }
    if (Objects.isNull(sort.getOrderFor(BaseEntity_.ID))) {
      sort = sort.and(Sort.by(Sort.Direction.ASC, BaseEntity_.ID));
    }
    if (!fetchRewardDataService.useKoios()) {
      sort =
          sort.stream()
              .filter(
                  order ->
                      !order.getProperty().equals("saturation")
                          && !order.getProperty().equals("poolSize"))
              .map(Sort::by)
              .findFirst()
              .orElse(defaultSort);
    }
    pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), sort);
    return pageable;
  }

  @Override
  public List<PoolResponse> findTopDelegationPool(Pageable pageable) {
    int size = pageable.getPageSize();
    if (size > defaultSize) {
      pageable = PageRequest.of(pageable.getPageNumber(), defaultSize);
    }

    List<PoolResponse> response;

    Integer currentEpoch =
        epochRepository
            .findCurrentEpochNo()
            .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));

    List<PoolCountProjection> poolCountProjections =
        blockRepository.findTopDelegationByEpochBlock(currentEpoch, pageable);
    Set<String> poolViewsTop =
        poolCountProjections.stream()
            .map(PoolCountProjection::getPoolView)
            .collect(Collectors.toSet());
    Map<Long, Integer> blockEpochsMap =
        poolCountProjections.stream()
            .collect(
                Collectors.toMap(
                    PoolCountProjection::getPoolId, PoolCountProjection::getCountValue));

    Set<Long> poolIds = poolHashRepository.getListPoolIdIn(poolViewsTop);
    List<PoolDelegationSummaryProjection> pools =
        delegationRepository.findDelegationPoolsSummary(poolIds);
    List<PoolCountProjection> blockLifetimeProjections =
        blockRepository.getCountBlockByPools(poolIds);
    Map<Long, Integer> blockLifetimesMap =
        blockLifetimeProjections.stream()
            .collect(
                Collectors.toMap(
                    PoolCountProjection::getPoolId, PoolCountProjection::getCountValue));
    response =
        pools.stream()
            .map(
                pool ->
                    PoolResponse.builder()
                        .poolId(pool.getPoolView())
                        .poolName(pool.getPoolName())
                        .feeAmount(pool.getFee())
                        .feePercent(pool.getMargin())
                        .pledge(pool.getPledge())
                        .id(pool.getPoolId())
                        .epochBlock(blockEpochsMap.get(pool.getPoolId()))
                        .lifetimeBlock(blockLifetimesMap.get(pool.getPoolId()))
                        .build())
            .toList();

    boolean useKoiOs = fetchRewardDataService.useKoios();
    if (useKoiOs) {
      setPoolInfoKoios(response, currentEpoch, poolViewsTop);
    }

    return response.stream()
        .sorted(Comparator.comparing(PoolResponse::getEpochBlock).reversed())
        .toList();
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolViewOrHash) {
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    boolean useKoiOs = fetchRewardDataService.useKoios();
    if (useKoiOs && !fetchRewardDataService.checkAdaPots(currentEpoch)) {
      fetchRewardDataService.fetchAdaPots(List.of(currentEpoch));
    }
    PoolDetailUpdateProjection projection =
        useKoiOs
            ? poolHashRepository.getDataForPoolDetail(poolViewOrHash, currentEpoch)
            : poolHashRepository.getDataForPoolDetailNoReward(poolViewOrHash, currentEpoch);
    String poolView = projection.getPoolView();
    Long poolId = projection.getPoolId();
    AggregatePoolInfo aggregatePoolInfo = aggregatePoolInfoRepository.findByPoolId(poolId);
    PoolDetailHeaderResponse poolDetailResponse =
        new PoolDetailHeaderResponse(projection, aggregatePoolInfo);
    Gson gson = new Gson();
    JsonObject jsonObject = gson.fromJson(projection.getJson(), JsonObject.class);
    if (Objects.nonNull(jsonObject) && jsonObject.has("homepage")) {
      poolDetailResponse.setHomepage(jsonObject.get("homepage").getAsString());
    }
    if (Objects.nonNull(jsonObject) && jsonObject.has("description")) {
      poolDetailResponse.setDescription(jsonObject.get("description").getAsString());
    }
    poolDetailResponse.setCreateDate(poolUpdateRepository.getCreatedTimeOfPool(poolId));

    List<String> ownerAddress = poolUpdateRepository.findOwnerAccountByPool(poolId);
    Collections.sort(ownerAddress);

    poolDetailResponse.setOwnerAccounts(ownerAddress);
    if (useKoiOs) {
      Set<String> poolIdList = new HashSet<>(Collections.singletonList(poolView));
      setPoolInfoKoios(poolDetailResponse, currentEpoch, poolIdList);
      if (!fetchRewardDataService.checkRewardForPool(ownerAddress)) {
        fetchRewardDataService.fetchRewardForPool(ownerAddress);
      }
      BigDecimal stakeLimit = getPoolSaturation(projection.getReserves(), projection.getParamK());
      poolDetailResponse.setStakeLimit(stakeLimit);
      BigInteger totalBalanceOfPoolOwners =
          stakeAddressRepository
              .getBalanceByView(ownerAddress)
              .add(rewardRepository.getAvailableRewardByAddressList(ownerAddress))
              .subtract(withdrawalRepository.getRewardWithdrawnByAddressList(ownerAddress));
      poolDetailResponse.setTotalBalanceOfPoolOwners(totalBalanceOfPoolOwners);
    }
    poolDetailResponse.setPoolStatus(poolCertificateService.getCurrentPoolStatus(poolViewOrHash));
    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(
      Pageable pageable, String poolViewOrHash) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    boolean useKoiOs = fetchRewardDataService.useKoios();
    if (!useKoiOs) {
      epochRes.setData(null);
      epochRes.setTotalItems(0);
      epochRes.setTotalPages(0);
      return epochRes;
    }
    List<PoolDetailEpochResponse> epochOfPools;
    PoolHash poolHash =
        poolHashRepository
            .findByViewOrHashRaw(poolViewOrHash)
            .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    String poolView = poolHash.getView();
    Page<PoolHistoryKoiosProjection> poolHistoryKoiosProjections = Page.empty();
    Boolean isHistory =
        fetchRewardDataService.checkPoolHistoryForPool(Collections.singleton(poolView));
    if (Boolean.FALSE.equals(isHistory)) {
      Boolean isFetch =
          fetchRewardDataService.fetchPoolHistoryForPool(Collections.singleton(poolView));
      if (Boolean.TRUE.equals(isFetch)) {
        poolHistoryKoiosProjections = poolHistoryRepository.getPoolHistoryKoios(poolView, pageable);
      }
    } else {
      poolHistoryKoiosProjections = poolHistoryRepository.getPoolHistoryKoios(poolView, pageable);
    }
    epochOfPools = poolHistoryKoiosProjections.stream().map(PoolDetailEpochResponse::new).toList();
    long totalElm = poolHistoryKoiosProjections.getTotalElements();
    Set<Integer> epochNos =
        poolHistoryKoiosProjections.stream()
            .map(PoolHistoryKoiosProjection::getEpochNo)
            .collect(Collectors.toSet());
    int totalPage = poolHistoryKoiosProjections.getTotalPages();
    Integer currentEpoch =
        epochRepository
            .findCurrentEpochNo()
            .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    List<PoolDetailEpochProjection> epochBlockProjections =
        poolHashRepository.findEpochByPool(poolId, epochNos);
    Map<Integer, Long> epochBlockMap =
        epochBlockProjections.stream()
            .collect(
                Collectors.toMap(
                    PoolDetailEpochProjection::getEpochNo,
                    PoolDetailEpochProjection::getCountBlock));
    epochOfPools.forEach(
        epochOfPool -> {
          epochOfPool.setBlock(epochBlockMap.get(epochOfPool.getEpoch()));
          if (epochOfPool.getEpoch().equals(currentEpoch - 1)) {
            epochOfPool.setDelegators(null);
            epochOfPool.setFee(null);
          }
        });
    epochRes.setData(epochOfPools);
    epochRes.setTotalItems(totalElm);
    epochRes.setTotalPages(totalPage);
    epochRes.setCurrentPage(pageable.getPageNumber());
    return epochRes;
  }

  @Override
  public PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String findByViewOrHashRaw) {
    boolean useKoiOs = fetchRewardDataService.useKoios();
    if (!useKoiOs) {
      return PoolDetailAnalyticsResponse.builder().epochChart(null).delegatorChart(null).build();
    }
    PoolHash poolHash =
        poolHashRepository
            .findByViewOrHashRaw(findByViewOrHashRaw)
            .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    String poolView = poolHash.getView();
    EpochChartResponse epochChart = new EpochChartResponse();
    List<EpochChartProjection> epochDataCharts = new ArrayList<>();
    Boolean isHistory =
        fetchRewardDataService.checkPoolHistoryForPool(Collections.singleton(poolView));
    if (Boolean.FALSE.equals(isHistory)) {
      Boolean isFetch =
          fetchRewardDataService.fetchPoolHistoryForPool(Collections.singleton(poolView));
      if (Boolean.TRUE.equals(isFetch)) {
        epochDataCharts = poolHistoryRepository.getPoolHistoryKoiosForEpochChart(poolView);
      }
    } else {
      epochDataCharts = poolHistoryRepository.getPoolHistoryKoiosForEpochChart(poolView);
    }
    if (!epochDataCharts.isEmpty()) {
      epochChart.setDataByDays(epochDataCharts.stream().map(EpochChartList::new).toList());
      Optional<EpochChartProjection> maxEpochOpt =
          epochDataCharts.stream().max(Comparator.comparing(EpochChartProjection::getChartValue));
      epochChart.setHighest(maxEpochOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
      Optional<EpochChartProjection> minEpochOpt =
          epochDataCharts.stream().min(Comparator.comparing(EpochChartProjection::getChartValue));
      epochChart.setLowest(minEpochOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
    }
    DelegatorChartResponse delegatorChart = getDelegatorChartResponse(poolHash);
    return PoolDetailAnalyticsResponse.builder()
        .epochChart(epochChart)
        .delegatorChart(delegatorChart)
        .build();
  }

  private DelegatorChartResponse getDelegatorChartResponse(PoolHash poolHash) {
    DelegatorChartResponse delegatorChart = new DelegatorChartResponse();
    List<DelegatorChartProjection> delegatorDataCharts =
        poolHistoryRepository.getDataForDelegatorChart(poolHash.getView());
    if (!delegatorDataCharts.isEmpty()) {
      delegatorChart.setDataByDays(
          delegatorDataCharts.stream().map(DelegatorChartList::new).toList());
      Optional<DelegatorChartProjection> maxDelegatorOpt =
          delegatorDataCharts.stream()
              .max(Comparator.comparing(DelegatorChartProjection::getChartValue));
      delegatorChart.setHighest(
          maxDelegatorOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
      Optional<DelegatorChartProjection> minDelegatorOpt =
          delegatorDataCharts.stream()
              .min(Comparator.comparing(DelegatorChartProjection::getChartValue));
      delegatorChart.setLowest(
          minDelegatorOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
    }
    return delegatorChart;
  }

  @Override
  public BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(
      Pageable pageable, String poolViewOrHash) {
    if (pageable.getSort().isUnsorted()) {
      pageable =
          PageRequest.of(
              pageable.getPageNumber(),
              pageable.getPageSize(),
              Sort.by(Sort.Direction.DESC, Delegation_.TX_ID));
    }
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    delegatorResponse.setData(List.of());
    Page<Long> addressIdPage = delegationRepository.liveDelegatorsList(poolViewOrHash, pageable);
    if (!addressIdPage.isEmpty()) {
      Set<Long> addressIds = addressIdPage.stream().collect(Collectors.toSet());
      Integer currentEpoch =
          epochRepository
              .findCurrentEpochNo()
              .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
      List<PoolDetailDelegatorProjection> delegatorPage =
          delegationRepository.getDelegatorsByAddress(addressIds);
      List<PoolDetailDelegatorResponse> delegatorList =
          delegatorPage.stream().map(PoolDetailDelegatorResponse::new).toList();
      boolean useKoiOs = fetchRewardDataService.useKoios();
      if (useKoiOs) {
        List<String> addressViews = stakeAddressRepository.getViewByAddressId(addressIds);
        Boolean isStake = fetchRewardDataService.checkEpochStakeForPool(addressViews);
        if (Boolean.FALSE.equals(isStake)) {
          Boolean isFetch =
              addressViews.size() > 40
                  ? null
                  : fetchRewardDataService.fetchEpochStakeForPool(addressViews);
          if (Objects.isNull(isFetch)) {
            List<CompletableFuture<Boolean>> completableFutures = new ArrayList<>();
            List<List<String>> subAddressList = Lists.partition(addressViews, 25);
            subAddressList.forEach(
                addressList -> completableFutures.add(fetchEpochStakeKoios(addressList)));
            CompletableFuture<Void> combinedFuture =
                CompletableFuture.allOf(completableFutures.toArray(new CompletableFuture[0]));
            CompletableFuture<List<Boolean>> allResultFuture =
                combinedFuture.thenApply(
                    v -> completableFutures.stream().map(CompletableFuture::join).toList());
            allResultFuture
                .thenApply(
                    results -> results.stream().allMatch(result -> result.equals(Boolean.TRUE)))
                .exceptionally(
                    ex -> {
                      log.error("Error: when fetch data from koios");
                      return Boolean.FALSE;
                    });
          }
        }
        List<StakeAddressProjection> stakeAddressProjections =
            epochStakeRepository.totalStakeByAddressAndPool(addressIds, currentEpoch);
        Map<Long, BigInteger> stakeAddressProjectionMap =
            stakeAddressProjections.stream()
                .collect(
                    Collectors.toMap(
                        StakeAddressProjection::getAddress, StakeAddressProjection::getTotalStake));
        delegatorList.forEach(
            delegator ->
                delegator.setTotalStake(
                    stakeAddressProjectionMap.get(delegator.getStakeAddressId())));
      }
      delegatorResponse.setTotalItems(addressIdPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
      delegatorResponse.setTotalPages(addressIdPage.getTotalPages());
      delegatorResponse.setCurrentPage(pageable.getPageNumber());
    }
    return delegatorResponse;
  }

  /**
   * calculate stake limit
   *
   * @return BigDecimal
   */
  private BigDecimal getPoolSaturation(BigInteger reserves, Integer k) {
    if (Objects.isNull(k) || Objects.isNull(reserves)) {
      return BigDecimal.ZERO;
    }
    return (CommonConstant.TOTAL_ADA.subtract(new BigDecimal(reserves)))
        .divide(new BigDecimal(k), CommonConstant.SCALE, RoundingMode.HALF_UP);
  }

  /**
   * set data for pool list from koios
   *
   * @return
   */
  private void setPoolInfoKoios(List<PoolResponse> poolList, Integer epochNo, Set<String> poolIds) {
    List<PoolInfoKoiosProjection> poolInfoProjections =
        poolInfoRepository.getPoolInfoKoios(poolIds, epochNo);
    Map<String, PoolInfoKoiosProjection> poolInfoMap =
        poolInfoProjections.stream()
            .collect(Collectors.toMap(PoolInfoKoiosProjection::getView, Function.identity()));
    poolList.parallelStream()
        .forEach(
            pool -> {
              PoolInfoKoiosProjection projection = poolInfoMap.get(pool.getPoolId());
              if (Objects.nonNull(projection)) {
                pool.setPoolSize(projection.getActiveStake());
                pool.setSaturation(projection.getSaturation());
              }
            });
  }

  /**
   * set data for pool from koios
   *
   * @return
   */
  private void setPoolInfoKoios(
      PoolDetailHeaderResponse poolDetailHeader, Integer epochNo, Set<String> poolIds) {
    List<PoolInfoKoiosProjection> poolInfoProjections =
        poolInfoRepository.getPoolInfoKoios(poolIds, epochNo);
    PoolInfoKoiosProjection projection =
        poolInfoProjections.isEmpty() ? null : poolInfoProjections.get(CommonConstant.ZERO);
    if (Objects.nonNull(projection)) {
      poolDetailHeader.setPoolSize(projection.getActiveStake());
      poolDetailHeader.setSaturation(projection.getSaturation());
    }
  }

  /**
   * fet epoch_stake from koios using multi thread
   *
   * @return CompletableFuture
   */
  private CompletableFuture<Boolean> fetchEpochStakeKoios(List<String> addressIds) {
    return CompletableFuture.supplyAsync(
        () -> fetchRewardDataService.fetchEpochStakeForPool(addressIds));
  }
}
