package org.cardanofoundation.explorer.api.service.impl;

import com.google.common.collect.Lists;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.address.DelegationPoolResponse;
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
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolActiveStakeProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolAmountProjection;
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
import org.cardanofoundation.explorer.api.repository.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolHistoryRepository;
import org.cardanofoundation.explorer.api.repository.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

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

  private final AdaPotsRepository adaPotsRepository;

  private final EpochParamRepository epochParamRepository;

  private final TxRepository txRepository;
  public static final int MILLI = 1000;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;

  @Value("${application.network}")
  private String network;

  @Override
  public BaseFilterResponse<DelegationResponse> getDelegations(Pageable pageable) {
    Page<Long> txIdPage = delegationRepository.findAllDelegations(pageable);
    List<TxIOProjection> txs = txRepository.findTxIn(txIdPage.getContent());
    Map<Long, TxIOProjection> txMap
        = txs.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));
    List<DelegationProjection> delegations = delegationRepository.findDelegationByTxIdIn(
        txIdPage.getContent());
    Map<Long, List<String>> delegationStakeKeyMap = delegations.stream()
        .collect(Collectors.groupingBy(DelegationProjection::getTxId,
            Collectors.mapping(DelegationProjection::getStakeAddress, Collectors.toList())));
    Map<Long, Set<DelegationPoolResponse>> delegationPoolMap = delegations.stream()
        .collect(Collectors.groupingBy(DelegationProjection::getTxId,
            Collectors.mapping(item ->
                DelegationPoolResponse.builder()
                    .poolName(item.getPoolName())
                    .tickerName(item.getTickerName())
                    .poolId(item.getPoolView())
                    .build(), Collectors.toSet())));
    List<DelegationResponse> responses = txIdPage.stream().map(
        item -> DelegationResponse.builder()
            .txHash(txMap.get(item).getHash())
            .blockNo(txMap.get(item).getBlockNo())
            .epochNo(txMap.get(item).getEpochNo())
            .epochSlotNo(txMap.get(item).getEpochSlotNo())
            .time(txMap.get(item).getTime())
            .epochSlotNo(txMap.get(item).getEpochSlotNo())
            .stakeKeys(delegationStakeKeyMap.get(item))
            .pools(delegationPoolMap.get(item).stream().toList())
            .build()
    ).toList();
    return new BaseFilterResponse<>(txIdPage, responses);
  }

  @Override
  public DelegationHeaderResponse getDataForDelegationHeader() {
    Epoch epoch = epochRepository.findByCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer epochNo = epoch.getNo();
    if (!fetchRewardDataService.checkAdaPots(epochNo)) {
      fetchRewardDataService.fetchAdaPots(List.of(epochNo));
    }
    Object poolActiveObj = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_ACTIVATE + network);
    Object poolInActiveObj = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_INACTIVATE + network);
    Timestamp startTime = epoch.getStartTime();
    Long slot = (Instant.now().toEpochMilli() - startTime.getTime()) / MILLI;
    long countDownTime =
        Timestamp.from(startTime.toInstant().plus(5, ChronoUnit.DAYS)).getTime() - Timestamp.from(
            Instant.now()).getTime();
    BigInteger liveStake;
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      log.info("using koiOs flow...");
      liveStake = poolInfoRepository.getTotalLiveStake(epochNo);
    } else {
      log.info("using base flow...");
      Object liveStakeObj = redisTemplate.opsForValue()
          .get(CommonConstant.REDIS_TOTAL_LIVE_STAKE + network);
      liveStake = Objects.nonNull(liveStakeObj) ? new BigInteger(String.valueOf(liveStakeObj))
          : BigInteger.ZERO;
    }
    Object delegatorCached = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_TOTAL_DELEGATOR + network);
    Integer delegators = Objects.nonNull(delegatorCached) ? Integer.parseInt(String.valueOf(delegatorCached)) : 0;
    return DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(slot)
        .liveStake(liveStake).delegators(delegators)
        .activePools(Objects.nonNull(poolActiveObj) ? (Integer) poolActiveObj : CommonConstant.ZERO)
        .retiredPools(
            Objects.nonNull(poolInActiveObj) ? (Integer) poolInActiveObj : CommonConstant.ZERO)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    if(Objects.nonNull(search)){
      search = search.toLowerCase();
    }
    if (search.isBlank()) {
      search = null;
    }
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(search,
        pageable);
    Integer epochNo = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    List<PoolResponse> poolList = new ArrayList<>();
    Set<Long> poolIds = new HashSet<>();
    List<Object> poolViews = new ArrayList<>();
    Set<String> poolIdList = new HashSet<>();
    poolIdPage.stream().forEach(pool -> {
      poolViews.add(pool.getPoolView());
      poolIdList.add(pool.getPoolView());
      poolList.add(PoolResponse.builder().poolId(pool.getPoolView())
          .id(pool.getPoolId())
          .poolName(pool.getPoolName())
          .pledge(pool.getPledge())
          .feeAmount(pool.getFee())
          .feePercent(pool.getMargin())
          .build());
      poolIds.add(pool.getPoolId());
    });
    List<PoolCountProjection> delegatorsCountProjections = delegationRepository.liveDelegatorsCountByPools(
        poolIds);
    Map<Long, Integer> numberDelegatorsMap = delegatorsCountProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId,
            PoolCountProjection::getCountValue));
    List<PoolCountProjection> blockLifetimeProjections = blockRepository.getCountBlockByPools(
        poolIds);
    Map<Long, Integer> blockLifetimesMap = blockLifetimeProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId,
            PoolCountProjection::getCountValue));
    List<PoolCountProjection> blockEpochProjections = blockRepository.getCountBlockByPoolsAndCurrentEpoch(
        poolIds, epochNo);
    Map<Long, Integer> blockEpochsMap = blockEpochProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId,
            PoolCountProjection::getCountValue));
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      setPoolInfoKoiOs(poolList, epochNo, poolIdList, numberDelegatorsMap, blockLifetimesMap,
          blockEpochsMap);
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolIdList);
      if (!isHistory) {
        fetchRewardDataService.fetchPoolHistoryForPool(poolIdList);
      }
      List<PoolHistoryKoiosProjection> poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(
          poolIdList, epochNo - 2);
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolIdList);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      if (!isReward) {
        fetchRewardDataService.fetchRewardForPool(rewardAccounts);
      }
      List<PoolAmountProjection> poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
          poolIdList, epochNo);
      setRewardKoiOs(poolHistoryProjections, poolAmountProjections, poolList);
    } else {
      Map<String, BigInteger> liveStakeMap = getStakeFromCache(
          CommonConstant.LIVE_STAKE, poolViews, null);
      Map<String, BigInteger> activeStakeMap = getStakeFromCache(
          CommonConstant.ACTIVATE_STAKE, poolViews, epochNo);
      List<PoolAmountProjection> poolAmountProjections = rewardRepository.getPoolRewardByPoolList(
          poolIds, epochNo);
      Map<Long, BigInteger> rewardMap = poolAmountProjections.stream()
          .collect(
              Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
      BigInteger reserves = adaPotsRepository.getReservesByEpochNo(epochNo);
      Integer paramK = epochParamRepository.getOptimalPoolCountByEpochNo(epochNo);
      BigDecimal stakeLimit = getPoolSaturation(reserves, paramK);
      poolList.forEach(
          pool -> {
            pool.setPoolSize(activeStakeMap.get(pool.getPoolId()));
            pool.setReward(getPoolRewardPercent(activeStakeMap.get(pool.getPoolId()),
                rewardMap.get(pool.getId())));
            pool.setSaturation(
                getSaturation(liveStakeMap.get(pool.getPoolId()), stakeLimit));
            pool.setNumberDelegators(numberDelegatorsMap.get(pool.getId()));
            pool.setLifetimeBlock(blockLifetimesMap.get(pool.getId()));
            pool.setEpochBlock(blockEpochsMap.get(pool.getId()));
          });
    }
    response.setData(poolList);
    response.setTotalItems(poolIdPage.getTotalElements());
    response.setTotalPages(poolIdPage.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  public List<PoolResponse> findTopDelegationPool(Pageable pageable) {
    int size = pageable.getPageSize();
    if (size > defaultSize) {
      pageable = PageRequest.of(pageable.getPageNumber(), defaultSize);
    }
    List<PoolResponse> response;
    Integer currentEpoch = epochRepository.findCurrentEpochNo()
        .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
    List<PoolCountProjection> poolCountProjections = blockRepository.findTopDelegationByEpochBlock(
        currentEpoch, pageable);
    Set<String> poolViewsTop = poolCountProjections.stream().map(PoolCountProjection::getPoolView)
        .collect(Collectors.toSet());
    Map<Long, Integer> blockEpochsMap = poolCountProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId, PoolCountProjection::getCountValue));
    List<PoolHistoryKoiosProjection> poolHistoryProjections = new ArrayList<>();
    List<PoolAmountProjection> poolAmountProjections = new ArrayList<>();
    Map<String, BigInteger> liveStakeMap = new HashMap<>();
    Map<String, BigInteger> activeStakeMap = new HashMap<>();
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (isKoiOs) {
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolViewsTop);
      if (!isHistory) {
        fetchRewardDataService.fetchPoolHistoryForPool(poolViewsTop);
      }
      poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolViewsTop,
          currentEpoch - 2);
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolViewsTop);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      if (!isReward) {
        fetchRewardDataService.fetchRewardForPool(rewardAccounts);
      }
      poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
          poolViewsTop, currentEpoch);
    } else {
      List<Object> objList = new ArrayList<>(poolViewsTop);
      activeStakeMap = getStakeFromCache(CommonConstant.ACTIVATE_STAKE,
          objList,
          currentEpoch);
      liveStakeMap = getStakeFromCache(
          CommonConstant.LIVE_STAKE, objList, null);
    }
    Set<Long> poolIds = poolHashRepository.getListPoolIdIn(poolViewsTop);
    List<PoolDelegationSummaryProjection> pools = delegationRepository.findDelegationPoolsSummary(
        poolIds);
    List<PoolCountProjection> blockLifetimeProjections = blockRepository.getCountBlockByPools(
        poolIds);
    Map<Long, Integer> blockLifetimesMap = blockLifetimeProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId,
            PoolCountProjection::getCountValue));
    response = pools.stream().map(pool ->
        PoolResponse.builder().poolId(pool.getPoolView()).poolName(pool.getPoolName())
            .feeAmount(pool.getFee())
            .feePercent(pool.getMargin()).pledge(pool.getPledge())
            .id(pool.getPoolId())
            .epochBlock(blockEpochsMap.get(pool.getPoolId()))
            .lifetimeBlock(blockLifetimesMap.get(pool.getPoolId()))
            .build()
    ).toList();
    if (isKoiOs) {
      setPoolInfoKoiOs(response, currentEpoch, poolViewsTop, null, null, null);
      setRewardKoiOs(poolHistoryProjections, poolAmountProjections, response);
    } else {
      List<PoolAmountProjection> poolRewardProjections = rewardRepository.getPoolRewardByPoolList(
          poolIds, currentEpoch);
      Map<Long, BigInteger> rewardMap = poolRewardProjections.stream()
          .collect(
              Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
      BigInteger reserves = adaPotsRepository.getReservesByEpochNo(currentEpoch);
      Integer paramK = epochParamRepository.getOptimalPoolCountByEpochNo(currentEpoch);
      BigDecimal stakeLimit = getPoolSaturation(reserves, paramK);
      for (PoolResponse pool : response) {
        Double saturation = getSaturation(liveStakeMap.get(pool.getPoolId()), stakeLimit);
        Double reward = getPoolRewardPercent(activeStakeMap.get(pool.getPoolId()),
            rewardMap.get(pool.getId()));
        pool.setPoolSize(activeStakeMap.get(pool.getPoolId()));
        pool.setSaturation(saturation);
        pool.setReward(reward);
      }
    }
    return response.stream().sorted(Comparator.comparing(PoolResponse::getEpochBlock).reversed())
        .toList();
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolView) {
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    if (!fetchRewardDataService.checkAdaPots(currentEpoch)) {
      fetchRewardDataService.fetchAdaPots(List.of(currentEpoch));
    }
    PoolDetailUpdateProjection projection = poolHashRepository.getDataForPoolDetail(
        poolView, currentEpoch);
    Long poolId = projection.getPoolId();
    PoolDetailHeaderResponse poolDetailResponse = Stream.of(projection)
        .map(PoolDetailHeaderResponse::new).findFirst()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    BigDecimal stakeLimit = getPoolSaturation(projection.getReserves(),
        projection.getParamK());
    poolDetailResponse.setStakeLimit(stakeLimit);
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Set<String> poolIdList = new HashSet<>(Collections.singletonList(poolView));
      setPoolInfoKoiOs(poolDetailResponse, currentEpoch, poolIdList);
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolIdList);
      if (!isHistory) {
        fetchRewardDataService.fetchPoolHistoryForPool(poolIdList);
      }
      List<PoolHistoryKoiosProjection> poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(
          poolIdList, currentEpoch - 2);
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolIdList);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      if (!isReward) {
        fetchRewardDataService.fetchRewardForPool(rewardAccounts);
      }
      List<PoolAmountProjection> poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
          poolIdList, currentEpoch);
      setRewardKoiOs(poolHistoryProjections, poolAmountProjections, poolDetailResponse);
    } else {
      List<Object> poolViews = new ArrayList<>();
      poolViews.add(poolView);
      Map<String, BigInteger> liveStakeMap = getStakeFromCache(
          CommonConstant.LIVE_STAKE, poolViews, null);
      Map<String, BigInteger> activeStakeMap = getStakeFromCache(
          CommonConstant.ACTIVATE_STAKE, poolViews, currentEpoch);
      poolDetailResponse.setPoolSize(activeStakeMap.get(poolView));
      poolDetailResponse.setSaturation(getSaturation(liveStakeMap.get(poolView), stakeLimit));
      BigInteger poolReward = rewardRepository.getPoolRewardByPool(poolId);
      poolDetailResponse.setReward(
          getPoolRewardPercent(activeStakeMap.get(poolView), poolReward));
      poolDetailResponse.setRos(
          getRos(poolReward, poolDetailResponse.getCost(), poolDetailResponse.getMargin(),
              activeStakeMap.get(poolView)));
    }
    poolDetailResponse.setCreateDate(poolUpdateRepository.getCreatedTimeOfPool(poolId));
    List<String> ownerAddress = poolUpdateRepository.findOwnerAccountByPool(poolId);
    Collections.sort(ownerAddress);
    poolDetailResponse.setOwnerAccounts(ownerAddress);
    poolDetailResponse.setDelegators(delegationRepository.liveDelegatorsCount(poolView));
    poolDetailResponse.setEpochBlock(blockRepository.getCountBlockByPoolAndCurrentEpoch(poolId));
    poolDetailResponse.setLifetimeBlock(blockRepository.getCountBlockByPool(poolId));
    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolView) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    List<PoolDetailEpochResponse> epochOfPools;
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    long totalElm;
    int totalPage;
    Set<Integer> epochNos;
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Page<PoolHistoryKoiosProjection> poolHistoryKoiOsProjections = Page.empty();
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(
          Collections.singleton(poolView));
      if (Boolean.FALSE.equals(isHistory)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(
            Collections.singleton(poolView));
        if (Boolean.TRUE.equals(isFetch)) {
          poolHistoryKoiOsProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolView,
              pageable);
        }
      } else {
        poolHistoryKoiOsProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolView, pageable);
      }
      epochOfPools = poolHistoryKoiOsProjections.stream().map(PoolDetailEpochResponse::new)
          .toList();
      totalElm = poolHistoryKoiOsProjections.getTotalElements();
      epochNos = poolHistoryKoiOsProjections.stream().map(PoolHistoryKoiosProjection::getEpochNo)
          .collect(
              Collectors.toSet());
      totalPage = poolHistoryKoiOsProjections.getTotalPages();
    } else {
      Page<PoolActiveStakeProjection> epochStakeProjections = epochStakeRepository.getDataForEpochList(
          poolId, pageable);
      epochOfPools = epochStakeProjections.stream().map(PoolDetailEpochResponse::new).toList();
      totalElm = epochStakeProjections.getTotalElements();
      epochNos = epochStakeProjections.stream().map(PoolActiveStakeProjection::getEpochNo)
          .collect(Collectors.toSet());
      totalPage = epochStakeProjections.getTotalPages();
      List<EpochRewardProjection> delegatorRewardProjections = rewardRepository.getDelegatorRewardByPool(
          poolId, epochNos);
      Map<Integer, BigInteger> delegatorRewardMap = delegatorRewardProjections.stream().collect(
          Collectors.toMap(EpochRewardProjection::getEpochNo, EpochRewardProjection::getAmount));
      List<EpochRewardProjection> poolRewardProjections = rewardRepository.getPoolRewardByPool(
          poolId, epochNos);
      Map<Integer, BigInteger> poolRewardMap = poolRewardProjections.stream().collect(
          Collectors.toMap(EpochRewardProjection::getEpochNo, EpochRewardProjection::getAmount));
      List<Epoch> epochs = epochRepository.findFeeByEpochNo(epochNos);
      Map<Integer, BigInteger> epochFeeMap = epochs.stream().collect(
          Collectors.toMap(Epoch::getNo, Epoch::getFees));
      PoolUpdate poolUpdate = poolUpdateRepository.findLastUpdateByPool(poolId);
      epochOfPools.forEach(epochOfPool -> {
        epochOfPool.setDelegators(delegatorRewardMap.get(epochOfPool.getEpoch()));
        epochOfPool.setFee(epochFeeMap.get(epochOfPool.getEpoch()));
        if (Objects.nonNull(poolUpdate)) {
          epochOfPool.setRos(
              getRos(poolRewardMap.get(epochOfPool.getEpoch()), poolUpdate.getFixedCost(),
                  poolUpdate.getMargin(),
                  epochOfPool.getStakeAmount()));
        }
      });
    }
    List<PoolDetailEpochProjection> epochBlockProjections = poolHashRepository.findEpochByPool(
        poolId,
        epochNos);
    Map<Integer, Long> epochBlockMap = epochBlockProjections.stream().collect(
        Collectors.toMap(PoolDetailEpochProjection::getEpochNo,
            PoolDetailEpochProjection::getCountBlock));
    epochOfPools.forEach(
        epochOfPool -> epochOfPool.setBlock(epochBlockMap.get(epochOfPool.getEpoch())));
    epochRes.setData(epochOfPools);
    epochRes.setTotalItems(totalElm);
    epochRes.setTotalPages(totalPage);
    epochRes.setCurrentPage(pageable.getPageNumber());
    return epochRes;
  }

  @Override
  public PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String poolView) {
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    EpochChartResponse epochChart = new EpochChartResponse();
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    List<EpochChartProjection> epochDataCharts = new ArrayList<>();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(
          Collections.singleton(poolView));
      if (Boolean.FALSE.equals(isHistory)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(
            Collections.singleton(poolView));
        if (Boolean.TRUE.equals(isFetch)) {
          epochDataCharts = poolHistoryRepository.getPoolHistoryKoiOsForEpochChart(poolView);
        }
      } else {
        epochDataCharts = poolHistoryRepository.getPoolHistoryKoiOsForEpochChart(poolView);
      }
    } else {
      epochDataCharts = epochStakeRepository.getDataForEpochChart(poolId);
    }
    if (!epochDataCharts.isEmpty()) {
      epochChart.setDataByDays(
          epochDataCharts.stream().map(EpochChartList::new).toList());
      Optional<EpochChartProjection> maxEpochOpt = epochDataCharts.stream()
          .max(Comparator.comparing(EpochChartProjection::getChartValue));
      epochChart.setHighest(maxEpochOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
      Optional<EpochChartProjection> minEpochOpt = epochDataCharts.stream()
          .min(Comparator.comparing(EpochChartProjection::getChartValue));
      epochChart.setLowest(minEpochOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
    }
    DelegatorChartResponse delegatorChart = new DelegatorChartResponse();
    List<DelegatorChartProjection> delegatorDataCharts = delegationRepository.getDataForDelegatorChart(
        poolId);
    if (!delegatorDataCharts.isEmpty()) {
      delegatorChart.setDataByDays(
          delegatorDataCharts.stream().map(DelegatorChartList::new).toList());
      Optional<DelegatorChartProjection> maxDelegatorOpt = delegatorDataCharts.stream()
          .max(Comparator.comparing(DelegatorChartProjection::getChartValue));
      delegatorChart.setHighest(
          maxDelegatorOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
      Optional<DelegatorChartProjection> minDelegatorOpt = delegatorDataCharts.stream()
          .min(Comparator.comparing(DelegatorChartProjection::getChartValue));
      delegatorChart.setLowest(
          minDelegatorOpt.map(BasePoolChartProjection::getChartValue).orElse(null));
    }
    return PoolDetailAnalyticsResponse.builder().epochChart(epochChart)
        .delegatorChart(delegatorChart).build();
  }

  @Override
  public BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(
      Pageable pageable, String poolView) {
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    Page<Long> addressIdPage = delegationRepository.liveDelegatorsList(poolView, pageable);
    if (!addressIdPage.isEmpty()) {
      Set<Long> addressIds = addressIdPage.stream().collect(Collectors.toSet());
      Integer currentEpoch = epochRepository.findCurrentEpochNo()
          .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
      List<PoolDetailDelegatorProjection> delegatorPage = delegationRepository.getDelegatorsByAddress(
          addressIds);
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).toList();
      Boolean isKoiOs = fetchRewardDataService.isKoiOs();
      boolean isCheck = true;
      if (Boolean.TRUE.equals(isKoiOs)) {
        List<String> addressViews = stakeAddressRepository.getViewByAddressId(addressIds);
        Boolean isStake = fetchRewardDataService.checkEpochStakeForPool(addressViews);
        if (Boolean.FALSE.equals(isStake)) {
          Boolean isFetch = addressViews.size() > 40 ? null
              : fetchRewardDataService.fetchEpochStakeForPool(addressViews);
          if (Objects.isNull(isFetch)) {
            List<CompletableFuture<Boolean>> completableFutures = new ArrayList<>();
            List<List<String>> subAddressList = Lists.partition(addressViews, 25);
            subAddressList.forEach(
                addressList -> completableFutures.add(fetchEpochStakeKoiOs(addressList)));
            CompletableFuture<Void> combinedFuture
                = CompletableFuture.allOf(
                completableFutures.toArray(new CompletableFuture[0]));
            CompletableFuture<List<Boolean>> allResultFuture = combinedFuture.thenApply(v ->
                completableFutures.stream().map(CompletableFuture::join)
                    .toList()
            );
            CompletableFuture<Boolean> resultFetch = allResultFuture.thenApply(results ->
                results.stream().allMatch(result -> result.equals(Boolean.TRUE))
            ).exceptionally(ex -> {
              log.error("Error: when fetch data from koios");
              return false;
            });
            try {
              isCheck = resultFetch.get();
            } catch (InterruptedException | ExecutionException e) {
              log.error("Error: " + e);
              Thread.currentThread().interrupt();
            }
          } else {
            isCheck = isFetch;
          }
        }
      }
      if (isCheck) {
        List<StakeAddressProjection> stakeAddressProjections = epochStakeRepository.totalStakeByAddressAndPool(
            addressIds, currentEpoch);
        Map<Long, BigInteger> stakeAddressProjectionMap = stakeAddressProjections.stream().collect(
            Collectors.toMap(StakeAddressProjection::getAddress,
                StakeAddressProjection::getTotalStake));
        delegatorList.forEach(delegator -> delegator.setTotalStake(
            stakeAddressProjectionMap.get(delegator.getStakeAddressId())));
      }
      delegatorResponse.setTotalItems(addressIdPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
      delegatorResponse.setTotalPages(addressIdPage.getTotalPages());
      delegatorResponse.setCurrentPage(pageable.getPageNumber());
    } else {
      delegatorResponse.setData(new ArrayList<>());
    }
    return delegatorResponse;
  }

  /**
   * calculate saturation
   *
   * @return Double
   */
  private Double getSaturation(BigInteger liveStake, BigDecimal poolSaturation) {
    if (poolSaturation.equals(BigDecimal.ZERO) || Objects.isNull(liveStake)) {
      return BigDecimal.ZERO.doubleValue();
    }
    return new BigDecimal(String.valueOf(liveStake)).divide(poolSaturation,
        CommonConstant.SCALE, RoundingMode.HALF_UP).multiply(CommonConstant.PERCENT).doubleValue();
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
    return (CommonConstant.TOTAL_ADA.subtract(new BigDecimal(reserves))).divide(new BigDecimal(k),
        CommonConstant.SCALE, RoundingMode.HALF_UP);
  }

  /**
   * set data for pool list from koios
   *
   * @return
   */
  private void setPoolInfoKoiOs(List<PoolResponse> poolList, Integer epochNo, Set<String> poolIds,
      Map<Long, Integer> numberDelegatorsMap, Map<Long, Integer> blockLifetimesMap,
      Map<Long, Integer> blockEpochsMap) {
    List<PoolInfoKoiosProjection> poolInfoProjections = poolInfoRepository.getPoolInfoKoiOs(
        poolIds, epochNo);
    Map<String, PoolInfoKoiosProjection> poolInfoMap = poolInfoProjections.stream()
        .collect(Collectors.toMap(PoolInfoKoiosProjection::getView, Function.identity()));
    poolList.forEach(pool -> {
      PoolInfoKoiosProjection projection = poolInfoMap.get(pool.getPoolId());
      if (Objects.nonNull(projection)) {
        pool.setPoolSize(projection.getActiveStake());
        pool.setSaturation(projection.getSaturation());
      }
      if (Objects.nonNull(numberDelegatorsMap)) {
        pool.setNumberDelegators(numberDelegatorsMap.get(pool.getId()));
      }
      if (Objects.nonNull(blockLifetimesMap)) {
        pool.setLifetimeBlock(blockLifetimesMap.get(pool.getId()));
      }
      if (Objects.nonNull(blockEpochsMap)) {
        pool.setEpochBlock(blockEpochsMap.get(pool.getId()));
      }
    });
  }

  /**
   * set reward data for pool list from koios
   *
   * @return
   */
  private void setRewardKoiOs(List<PoolHistoryKoiosProjection> poolHistoryProjections,
      List<PoolAmountProjection> poolAmountProjections, List<PoolResponse> poolList) {
    Map<String, PoolHistoryKoiosProjection> poolHistoryMap = poolHistoryProjections.stream()
        .collect(Collectors.toMap(PoolHistoryKoiosProjection::getView, Function.identity()));
    Map<String, PoolAmountProjection> operatorRewardMap = poolAmountProjections.stream()
        .collect(Collectors.toMap(PoolAmountProjection::getView, Function.identity()));
    poolList.forEach(pool -> {
      PoolHistoryKoiosProjection delegateRewardProjection = poolHistoryMap.get(pool.getPoolId());
      BigInteger delegateReward = BigInteger.ZERO;
      if (Objects.nonNull(delegateRewardProjection) && Objects.nonNull(
          delegateRewardProjection.getDelegateReward())) {
        delegateReward = delegateRewardProjection.getDelegateReward();
        pool.setLifetimeRos(delegateRewardProjection.getRos());
      }
      PoolAmountProjection operatorRewardProjection = operatorRewardMap.get(pool.getPoolId());
      BigInteger operatorReward = BigInteger.ZERO;
      if (Objects.nonNull(operatorRewardProjection) && Objects.nonNull(
          operatorRewardProjection.getAmount())) {
        operatorReward = operatorRewardProjection.getAmount();
      }
      BigInteger poolReward = delegateReward.add(operatorReward);
      pool.setReward(getPoolRewardPercent(pool.getPoolSize(), poolReward));
    });
  }

  /**
   * calculate pool reward
   *
   * @return Double
   */
  private Double getPoolRewardPercent(BigInteger activeStake, BigInteger poolReward) {
    if (Objects.isNull(activeStake) || Objects.isNull(poolReward)
        || BigInteger.ZERO.compareTo(activeStake) == 0) {
      return BigInteger.ZERO.doubleValue();
    }
    return new BigDecimal(poolReward).divide(new BigDecimal(activeStake), CommonConstant.SCALE,
            RoundingMode.HALF_UP).multiply(CommonConstant.EPOCH_IN_YEARS)
        .multiply(CommonConstant.PERCENT).doubleValue();
  }

  /**
   * get stake from redis
   *
   * @return Map
   */
  private Map<String, BigInteger> getStakeFromCache(String prefixKey, List<Object> poolIds,
      Integer epochNo) {
    Map<String, BigInteger> stakeFromCache = new HashMap<>();
    if (Objects.isNull(poolIds)) {
      return stakeFromCache;
    }
    String key = prefixKey + network + (Objects.isNull(epochNo) ? "" : ("_" + epochNo));
    List<Object> objStakeList = null;
    try {
      objStakeList = redisTemplate.opsForHash().multiGet(key, poolIds);
    } catch (Exception e) {
      log.info("Error when get stake from cache with Key=" + key);
      return stakeFromCache;
    }
    if (!objStakeList.isEmpty()) {
      int i = 0;
      for (Object poolId : poolIds) {
        Object objStake = objStakeList.get(i);
        if (Objects.nonNull(objStake)
            && new BigInteger(String.valueOf(objStake)).compareTo(BigInteger.ZERO) > 0) {
          stakeFromCache.put((String) poolId, (BigInteger) objStake);
        } else {
          stakeFromCache.put((String) poolId, BigInteger.ZERO);
        }
        i++;
      }
    }
    return stakeFromCache;
  }

  /**
   * set data for pool from koios
   *
   * @return
   */
  private void setPoolInfoKoiOs(PoolDetailHeaderResponse poolDetailHeader, Integer epochNo,
      Set<String> poolIds) {
    List<PoolInfoKoiosProjection> poolInfoProjections = poolInfoRepository.getPoolInfoKoiOs(
        poolIds, epochNo);
    PoolInfoKoiosProjection projection =
        poolInfoProjections.isEmpty() ? null : poolInfoProjections.get(CommonConstant.ZERO);
    if (Objects.nonNull(projection)) {
      poolDetailHeader.setPoolSize(projection.getActiveStake());
      poolDetailHeader.setSaturation(projection.getSaturation());
    }
  }

  /**
   * set reward data for pool from koios
   *
   * @return
   */
  private void setRewardKoiOs(List<PoolHistoryKoiosProjection> poolHistoryProjections,
      List<PoolAmountProjection> poolAmountProjections, PoolDetailHeaderResponse poolDetailHeader) {
    PoolHistoryKoiosProjection poolHistoryKoiOsProjection =
        poolHistoryProjections.isEmpty() ? null : poolHistoryProjections.get(CommonConstant.ZERO);
    PoolAmountProjection poolAmountProjection =
        poolAmountProjections.isEmpty() ? null : poolAmountProjections.get(CommonConstant.ZERO);
    BigInteger delegateReward = BigInteger.ZERO;
    if (Objects.nonNull(poolHistoryKoiOsProjection) && Objects.nonNull(
        poolHistoryKoiOsProjection.getDelegateReward())) {
      poolDetailHeader.setRos(poolHistoryKoiOsProjection.getRos());
      delegateReward = poolHistoryKoiOsProjection.getDelegateReward();
    }
    BigInteger operatorReward = BigInteger.ZERO;
    if (Objects.nonNull(poolAmountProjection) && Objects.nonNull(
        poolAmountProjection.getAmount())) {
      operatorReward = poolAmountProjection.getAmount();
    }
    BigInteger poolReward = delegateReward.add(operatorReward);
    poolDetailHeader.setReward(getPoolRewardPercent(poolDetailHeader.getPoolSize(), poolReward));
  }

  /**
   * calculate ros
   *
   * @return Double
   */
  private Double getRos(BigInteger poolReward, BigInteger fixedCost, Double margin,
      BigInteger activeStake) {
    if (Objects.isNull(poolReward) || Objects.isNull(fixedCost) || Objects.isNull(margin)
        || Objects.isNull(activeStake)) {
      return BigInteger.ZERO.doubleValue();
    }
    BigDecimal distributedReward = new BigDecimal(poolReward).subtract(new BigDecimal(fixedCost));
    BigDecimal poolFee = distributedReward.multiply(BigDecimal.valueOf(margin));
    distributedReward = distributedReward.subtract(poolFee);
    return distributedReward.divide(new BigDecimal(activeStake), CommonConstant.SCALE,
            RoundingMode.HALF_UP).multiply(CommonConstant.EPOCH_IN_YEARS)
        .multiply(CommonConstant.PERCENT).doubleValue();
  }

  /**
   * fet epoch_stake from koios using multi thread
   *
   * @return CompletableFuture
   */
  private CompletableFuture<Boolean> fetchEpochStakeKoiOs(List<String> addressIds) {
    return CompletableFuture.supplyAsync(
        () -> fetchRewardDataService.fetchEpochStakeForPool(addressIds));
  }
}