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
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
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
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochStakeProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolAmountProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiOsProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoKoiOsProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolHistoryRepository;
import org.cardanofoundation.explorer.api.repository.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
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
  public static final int MILLI = 1000;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;

  @Value("${application.network}")
  private String network;

  @Override
  public DelegationHeaderResponse getDataForDelegationHeader() {
    Epoch epoch = epochRepository.findByCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer epochNo = epoch.getNo();
    Timestamp startTime = epoch.getStartTime();
    Long slot = (Instant.now().toEpochMilli() - startTime.getTime()) / MILLI;
    long countDownTime =
        Timestamp.from(startTime.toInstant().plus(5, ChronoUnit.DAYS)).getTime() - Timestamp.from(
            Instant.now()).getTime();
    BigInteger liveStake = null;
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      log.info("using koiOs flow...");
      Set<String> poolIds = poolHashRepository.findAllSetPoolView();
      if (Boolean.FALSE.equals(fetchRewardDataService.checkPoolInfoForPool(poolIds))) {
        if (Boolean.TRUE.equals(fetchRewardDataService.fetchPoolInfoForPool(poolIds))) {
          liveStake = poolInfoRepository.getTotalLiveStake(epochNo);
        }
      } else {
        liveStake = poolInfoRepository.getTotalLiveStake(epochNo);
      }
    } else {
      log.info("using base flow...");
      Object liveStakeObj = redisTemplate.opsForValue()
          .get(CommonConstant.REDIS_TOTAL_LIVE_STAKE + network);
      liveStake = Objects.nonNull(liveStakeObj) ? new BigInteger(String.valueOf(liveStakeObj))
          : BigInteger.ZERO;
    }
    Integer delegators = delegationRepository.numberDelegatorsAllPoolByEpochNo(
        Long.valueOf(epochNo));
    return DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(slot)
        .liveStake(liveStake).delegators(delegators)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    if (Objects.nonNull(search) && search.isBlank()) {
      search = null;
    }
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(search,
        search, pageable);
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
          .stakeLimit(getPoolSaturation(pool.getReserves(), pool.getParamK()))
          .build());
      poolIds.add(pool.getPoolId());
    });
    Integer epochNo = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Boolean isInfo = fetchRewardDataService.checkPoolInfoForPool(poolIdList);
      if (Boolean.FALSE.equals(isInfo)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolInfoForPool(poolIdList);
        if (Boolean.TRUE.equals(isFetch)) {
          setPoolInfoKoiOs(poolList, epochNo, poolIdList);
        }
      } else {
        setPoolInfoKoiOs(poolList, epochNo, poolIdList);
      }
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolIdList);
      List<PoolHistoryKoiOsProjection> poolHistoryProjections = new ArrayList<>();
      if (Boolean.FALSE.equals(isHistory)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(poolIdList);
        if (Boolean.TRUE.equals(isFetch)) {
          poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolIdList,
              epochNo - 2);
        }
      } else {
        poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolIdList, epochNo - 2);
      }
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolIdList);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      List<PoolAmountProjection> poolAmountProjections = new ArrayList<>();
      if (Boolean.FALSE.equals(isReward)) {
        Boolean isFetch = fetchRewardDataService.fetchRewardForPool(rewardAccounts);
        if (Boolean.TRUE.equals(isFetch)) {
          poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
              poolIdList, epochNo);
        }
      } else {
        poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
            poolIdList, epochNo);
      }
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
      poolList.forEach(
          pool -> {
            pool.setPoolSize(activeStakeMap.get(pool.getPoolId()));
            pool.setReward(getPoolRewardPercent(activeStakeMap.get(pool.getPoolId()),
                rewardMap.get(pool.getId())));
            pool.setSaturation(
                getSaturation(liveStakeMap.get(pool.getPoolId()),
                    pool.getStakeLimit()));
          });
    }
    response.setData(poolList);
    response.setTotalItems(poolIdPage.getTotalElements());
    return response;
  }

  @Override
  public List<PoolResponse> findTopDelegationPool(Pageable pageable) {

    int size = defaultSize;
    if (pageable.getPageSize() < defaultSize) {
      size = pageable.getPageSize();
    }
    List<PoolResponse> response = new ArrayList<>();
    Integer currentEpoch = epochRepository.findCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Set<String> poolViewsTop;
    List<PoolHistoryKoiOsProjection> poolHistoryProjections = new ArrayList<>();
    List<PoolAmountProjection> poolAmountProjections = new ArrayList<>();
    Map<String, BigInteger> liveStakeMap = new HashMap<>();
    Map<String, BigInteger> activeStakeMap = new HashMap<>();
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Set<String> poolViews = poolHashRepository.findAllSetPoolView();
      List<PoolInfoKoiOsProjection> poolInfoProjections;
      Boolean isInfo = fetchRewardDataService.checkPoolInfoForPool(poolViews);
      if (Boolean.FALSE.equals(isInfo)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolInfoForPool(poolViews);
        if (Boolean.TRUE.equals(isFetch)) {
          poolInfoProjections = poolInfoRepository.getTopPoolInfoKoiOs(currentEpoch, pageable);
        } else {
          return response;
        }
      } else {
        poolInfoProjections = poolInfoRepository.getTopPoolInfoKoiOs(currentEpoch, pageable);
      }
      poolViewsTop = poolInfoProjections.stream().map(PoolInfoKoiOsProjection::getView).collect(
          Collectors.toSet());
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolViewsTop);
      if (Boolean.FALSE.equals(isHistory)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(poolViewsTop);
        if (Boolean.TRUE.equals(isFetch)) {
          poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolViewsTop,
              currentEpoch - 2);
        }
      } else {
        poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolViewsTop,
            currentEpoch - 2);
      }
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolViewsTop);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      if (Boolean.FALSE.equals(isReward)) {
        Boolean isFetch = fetchRewardDataService.fetchRewardForPool(rewardAccounts);
        if (Boolean.TRUE.equals(isFetch)) {
          poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
              poolViewsTop, currentEpoch);
        }
      } else {
        poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
            poolViewsTop, currentEpoch);
      }
    } else {
      List<Object> poolViews = poolHashRepository.findAllPoolView();
      activeStakeMap = getStakeFromCache(CommonConstant.ACTIVATE_STAKE,
          poolViews,
          currentEpoch);
      poolViewsTop = activeStakeMap.entrySet().stream()
          .sorted(Map.Entry.<String, BigInteger>comparingByValue().reversed()).limit(size)
          .map(Map.Entry::getKey).collect(Collectors.toSet());
      List<Object> objList = new ArrayList<>(poolViewsTop);
      liveStakeMap = getStakeFromCache(
          CommonConstant.LIVE_STAKE, objList, null);
    }
    Set<Long> poolIds = poolHashRepository.getListPoolIdIn(poolViewsTop);
    List<PoolDelegationSummaryProjection> pools = delegationRepository.findDelegationPoolsSummary(
        poolIds);
    response = pools.stream().map(pool ->
        PoolResponse.builder().poolId(pool.getPoolView()).poolName(pool.getPoolName())
            .kParam(pool.getOptimalPoolCount())
            .reserves(pool.getReserves())
            .feeAmount(pool.getFee())
            .feePercent(pool.getMargin()).pledge(pool.getPledge())
            .id(pool.getPoolId())
            .build()
    ).toList();
    if (Boolean.TRUE.equals(isKoiOs)) {
      setPoolInfoKoiOs(response, currentEpoch, poolViewsTop);
      setRewardKoiOs(poolHistoryProjections, poolAmountProjections, response);
    } else {
      List<PoolAmountProjection> poolRewardProjections = rewardRepository.getPoolRewardByPoolList(
          poolIds, currentEpoch);
      Map<Long, BigInteger> rewardMap = poolRewardProjections.stream()
          .collect(
              Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
      for (PoolResponse pool : response) {
        Double saturation = getSaturation(liveStakeMap.get(pool.getPoolId()),
            getPoolSaturation(pool.getReserves(), pool.getKParam()));
        Double reward = getPoolRewardPercent(activeStakeMap.get(pool.getPoolId()),
            rewardMap.get(pool.getId()));
        pool.setPoolSize(activeStakeMap.get(pool.getPoolId()));
        pool.setSaturation(saturation);
        pool.setReward(reward);
      }
    }
    return response.stream().sorted(Comparator.comparing(PoolResponse::getPoolSize).reversed())
        .toList();
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolView) {
    PoolDetailUpdateProjection projection = poolHashRepository.getDataForPoolDetail(
        poolView);
    Long poolId = projection.getPoolId();
    PoolDetailHeaderResponse poolDetailResponse = Stream.of(projection)
        .map(PoolDetailHeaderResponse::new).findFirst()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer currentEpoch = epochRepository.findCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    BigDecimal stakeLimit = getPoolSaturation(projection.getReserves(),
        projection.getParamK());
    poolDetailResponse.setStakeLimit(stakeLimit);
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Set<String> poolIdList = new HashSet<>();
      poolIdList.add(poolView);
      Boolean isInfo = fetchRewardDataService.checkPoolInfoForPool(poolIdList);
      if (Boolean.FALSE.equals(isInfo)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolInfoForPool(poolIdList);
        if (Boolean.TRUE.equals(isFetch)) {
          setPoolInfoKoiOs(poolDetailResponse, currentEpoch, poolIdList);
        }
      } else {
        setPoolInfoKoiOs(poolDetailResponse, currentEpoch, poolIdList);
      }
      Boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolIdList);
      List<PoolHistoryKoiOsProjection> poolHistoryProjections = new ArrayList<>();
      if (Boolean.FALSE.equals(isHistory)) {
        Boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(poolIdList);
        if (Boolean.TRUE.equals(isFetch)) {
          poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolIdList,
              currentEpoch - 2);
        }
      } else {
        poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolIdList,
            currentEpoch - 2);
      }
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolIdList);
      Boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
      List<PoolAmountProjection> poolAmountProjections = new ArrayList<>();
      if (Boolean.FALSE.equals(isReward)) {
        Boolean isFetch = fetchRewardDataService.fetchRewardForPool(rewardAccounts);
        if (Boolean.TRUE.equals(isFetch)) {
          poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
              poolIdList, currentEpoch);

        }
      } else {
        poolAmountProjections = rewardRepository.getOperatorRewardByPoolList(
            poolIdList, currentEpoch);
      }
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
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
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
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    long totalElm;
    Set<Integer> epochNos;
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (Boolean.TRUE.equals(isKoiOs)) {
      Page<PoolHistoryKoiOsProjection> poolHistoryKoiOsProjections = Page.empty();
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
      epochNos = poolHistoryKoiOsProjections.stream().map(PoolHistoryKoiOsProjection::getEpochNo)
          .collect(
              Collectors.toSet());
    } else {
      Page<EpochStakeProjection> epochStakeProjections = epochStakeRepository.getDataForEpochList(
          poolId, pageable);
      epochOfPools = epochStakeProjections.stream().map(PoolDetailEpochResponse::new).toList();
      totalElm = epochStakeProjections.getTotalElements();
      epochNos = epochStakeProjections.stream().map(EpochStakeProjection::getEpochNo)
          .collect(Collectors.toSet());
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
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    Page<PoolDetailDelegatorProjection> delegatorPage = delegationRepository.getAllDelegatorByPool(
        poolId, pageable);
    if (!delegatorPage.isEmpty()) {
      Integer currentEpoch = epochRepository.findCurrentEpochNo()
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).toList();
      Set<Long> addressIds = delegatorList.stream()
          .map(PoolDetailDelegatorResponse::getStakeAddressId).collect(Collectors.toSet());
      Boolean isKoiOs = fetchRewardDataService.isKoiOs();
      boolean isCheck = true;
      if (Boolean.TRUE.equals(isKoiOs)) {
        List<String> addressViews = stakeAddressRepository.getViewByAddressId(addressIds);
        Boolean isStake = fetchRewardDataService.checkEpochStakeForPool(addressViews);
        if (Boolean.FALSE.equals(isStake)) {
          Boolean isFetch = addressViews.size() > 25 ? null
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
            addressIds, poolId, currentEpoch);
        Map<Long, BigInteger> stakeAddressProjectionMap = stakeAddressProjections.stream().collect(
            Collectors.toMap(StakeAddressProjection::getAddress,
                StakeAddressProjection::getTotalStake));
        delegatorList.forEach(delegator -> delegator.setTotalStake(
            stakeAddressProjectionMap.get(delegator.getStakeAddressId())));
      }
      delegatorResponse.setTotalItems(delegatorPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
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
  private void setPoolInfoKoiOs(List<PoolResponse> poolList, Integer epochNo, Set<String> poolIds) {
    List<PoolInfoKoiOsProjection> poolInfoProjections = poolInfoRepository.getPoolInfoKoiOs(
        poolIds, epochNo);
    Map<String, PoolInfoKoiOsProjection> poolInfoMap = poolInfoProjections.stream()
        .collect(Collectors.toMap(PoolInfoKoiOsProjection::getView, Function.identity()));
    poolList.forEach(pool -> {
      PoolInfoKoiOsProjection projection = poolInfoMap.get(pool.getPoolId());
      if (Objects.nonNull(projection)) {
        pool.setPoolSize(projection.getActiveStake());
        pool.setSaturation(projection.getSaturation());
      }
    });
  }

  /**
   * set reward data for pool list from koios
   *
   * @return
   */
  private void setRewardKoiOs(List<PoolHistoryKoiOsProjection> poolHistoryProjections,
      List<PoolAmountProjection> poolAmountProjections, List<PoolResponse> poolList) {
    Map<String, PoolHistoryKoiOsProjection> poolHistoryMap = poolHistoryProjections.stream()
        .collect(Collectors.toMap(PoolHistoryKoiOsProjection::getView, Function.identity()));
    Map<String, PoolAmountProjection> operatorRewardMap = poolAmountProjections.stream()
        .collect(Collectors.toMap(PoolAmountProjection::getView, Function.identity()));
    poolList.forEach(pool -> {
      PoolHistoryKoiOsProjection delegateRewardProjection = poolHistoryMap.get(pool.getPoolId());
      BigInteger delegateReward = BigInteger.ZERO;
      if (Objects.nonNull(delegateRewardProjection) && Objects.nonNull(
          delegateRewardProjection.getDelegateReward())) {
        delegateReward = delegateRewardProjection.getDelegateReward();
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
    List<PoolInfoKoiOsProjection> poolInfoProjections = poolInfoRepository.getPoolInfoKoiOs(
        poolIds, epochNo);
    PoolInfoKoiOsProjection projection =
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
  private void setRewardKoiOs(List<PoolHistoryKoiOsProjection> poolHistoryProjections,
      List<PoolAmountProjection> poolAmountProjections, PoolDetailHeaderResponse poolDetailHeader) {
    PoolHistoryKoiOsProjection poolHistoryKoiOsProjection =
        poolHistoryProjections.isEmpty() ? null : poolHistoryProjections.get(CommonConstant.ZERO);
    PoolAmountProjection poolAmountProjection =
        poolAmountProjections.isEmpty() ? null : poolAmountProjections.get(CommonConstant.ZERO);
    if (Objects.nonNull(poolHistoryKoiOsProjection)) {
      BigInteger delegateReward = BigInteger.ZERO;
      poolDetailHeader.setRos(poolHistoryKoiOsProjection.getRos());
      if (Objects.nonNull(
          poolHistoryKoiOsProjection.getDelegateReward())) {
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
