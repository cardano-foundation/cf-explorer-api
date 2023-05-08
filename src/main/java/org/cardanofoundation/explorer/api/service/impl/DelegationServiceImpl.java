package org.cardanofoundation.explorer.api.service.impl;

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
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
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

  private final RedisTemplate<String, Object> redisTemplate;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;


  @Override
  public DelegationHeaderResponse getDataForDelegationHeader() {
    Epoch epoch = epochRepository.findByCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer epochNo = epoch.getNo();
    Timestamp endTime = epoch.getEndTime();
    long countDownTime = endTime.getTime() - Timestamp.from(Instant.now()).getTime();
    Integer currentSlot = blockRepository.findCurrentSlotByEpochNo(epochNo);
    Object liveStake = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_TOTAL_LIVE_STAKE);
    Integer delegators = delegationRepository.numberDelegatorsAllPoolByEpochNo(
        Long.valueOf(epochNo));
    return DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(currentSlot)
        .liveStake(Objects.nonNull(liveStake) ? new BigInteger(String.valueOf(liveStake))
            : BigInteger.ZERO).delegators(delegators)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(search,
        search, pageable);
    List<PoolResponse> poolList = new ArrayList<>();
    Set<Long> poolIds = new HashSet<>();
    poolIdPage.stream().forEach(pool -> {
      poolList.add(PoolResponse.builder().poolId(pool.getPoolView())
          .id(pool.getPoolId())
          .poolName(pool.getPoolName())
          .pledge(pool.getPledge())
          .feeAmount(pool.getFee())
          .feePercent(pool.getMargin())
          .saturation(getSaturation(pool.getPoolView(),
              getPoolSaturation(pool.getReserves(), pool.getParamK())))
          .build());
      poolIds.add(pool.getPoolId());
    });
    List<PoolAmountProjection> poolActiveProjections = epochStakeRepository.activeStakeByPoolList(
        poolIds);
    Map<Long, BigInteger> activeStakeMap = poolActiveProjections.stream()
        .collect(
            Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
    List<PoolAmountProjection> poolAmountProjections = rewardRepository.getPoolRewardByPoolList(
        poolIds);
    Map<Long, BigInteger> rewardMap = poolAmountProjections.stream()
        .collect(
            Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
    poolList.forEach(
        pool -> {
          pool.setPoolSize(activeStakeMap.get(pool.getId()));
          pool.setReward(
              getPoolRewardPercent(activeStakeMap.get(pool.getId()), rewardMap.get(pool.getId())));
        });
    response.setData(poolList);
    response.setTotalItems(poolIdPage.getTotalElements());
    return response;
  }

  @Override
  public Set<PoolResponse> findTopDelegationPool(Pageable pageable) {

    if (pageable.getPageSize() > defaultSize) {
      pageable = PageRequest.of(0, defaultSize);
    }
    List<PoolAmountProjection> topPoolProjections = epochStakeRepository.findTopPoolSize(pageable);
    Map<Long, BigInteger> activeStakeMap = topPoolProjections.stream()
        .collect(
            Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
    Set<Long> poolIds = topPoolProjections.stream().map(PoolAmountProjection::getPoolId)
        .collect(
            Collectors.toSet());
    List<PoolDelegationSummaryProjection> pools = delegationRepository.findDelegationPoolsSummary(
        poolIds);
    List<PoolAmountProjection> poolAmountProjections = rewardRepository.getPoolRewardByPoolList(
        poolIds);
    Map<Long, BigInteger> rewardMap = poolAmountProjections.stream()
        .collect(
            Collectors.toMap(PoolAmountProjection::getPoolId, PoolAmountProjection::getAmount));
    return pools.stream().map(pool -> {
          Integer paramK = pool.getOptimalPoolCount();
          Double saturation = getSaturation(pool.getPoolView(),
              getPoolSaturation(pool.getReserves(), paramK));
          return PoolResponse.builder().poolId(pool.getPoolView()).poolName(pool.getPoolName())
              .poolSize(activeStakeMap.get(pool.getPoolId()))
              .saturation(saturation).feeAmount(pool.getFee())
              .feePercent(pool.getMargin()).pledge(pool.getPledge())
              .reward(getPoolRewardPercent(activeStakeMap.get(pool.getPoolId()),
                  rewardMap.get(pool.getPoolId()))).build();
        }).sorted(((o1, o2) -> o2.getPoolSize().compareTo(o1.getPoolSize())))
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolView) {
    PoolDetailUpdateProjection projection = poolHashRepository.getDataForPoolDetail(
        poolView);
    Long poolId = projection.getPoolId();
    PoolDetailHeaderResponse poolDetailResponse = Stream.of(projection)
        .map(PoolDetailHeaderResponse::new).findFirst()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    BigInteger activeStake = epochStakeRepository.activeStakeByPool(poolId);
    poolDetailResponse.setPoolSize(activeStake);
    poolDetailResponse.setCreateDate(poolUpdateRepository.getCreatedTimeOfPool(poolId));
    List<String> ownerAddress = poolUpdateRepository.findOwnerAccountByPool(poolId);
    Collections.sort(ownerAddress);
    poolDetailResponse.setOwnerAccounts(ownerAddress);
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    BigDecimal stakeLimit = getPoolSaturation(projection.getReserves(),
        projection.getParamK());
    poolDetailResponse.setSaturation(getSaturation(poolView, stakeLimit));
    poolDetailResponse.setStakeLimit(stakeLimit);
    BigInteger poolReward = rewardRepository.getPoolRewardByPool(poolId);
    poolDetailResponse.setReward(getPoolRewardPercent(activeStake, poolReward));
    poolDetailResponse.setRos(0.0);//todo move sprint 3
    poolDetailResponse.setEpochBlock(blockRepository.getCountBlockByPoolAndCurrentEpoch(poolId));
    poolDetailResponse.setLifetimeBlock(blockRepository.getCountBlockByPool(poolId));
    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolView) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    Page<EpochStakeProjection> epochStakeProjections = epochStakeRepository.getDataForEpochList(
        poolId, pageable);
    List<PoolDetailEpochResponse> epochOfPools = epochStakeProjections.stream()
        .map(PoolDetailEpochResponse::new).collect(Collectors.toList());
    Set<Integer> epochNos = epochStakeProjections.stream().map(EpochStakeProjection::getEpochNo)
        .collect(Collectors.toSet());
    List<PoolDetailEpochProjection> epochBlockProjections = poolHashRepository.findEpochByPool(
        poolId,
        epochNos);
    Map<Integer, Long> epochBlockMap = epochBlockProjections.stream().collect(
        Collectors.toMap(PoolDetailEpochProjection::getEpochNo,
            PoolDetailEpochProjection::getCountBlock));
    List<EpochRewardProjection> epochRewardProjections = rewardRepository.getDelegatorRewardByPool(
        poolId, epochNos);
    Map<Integer, BigInteger> epochRewardMap = epochRewardProjections.stream().collect(
        Collectors.toMap(EpochRewardProjection::getEpochNo, EpochRewardProjection::getAmount));
    List<Epoch> epochs = epochRepository.findFeeByEpochNo(epochNos);
    Map<Integer, BigInteger> epochFeeMap = epochs.stream().collect(
        Collectors.toMap(Epoch::getNo, Epoch::getFees));
    epochOfPools.forEach(epochOfPool -> {
      epochOfPool.setBlock(epochBlockMap.get(epochOfPool.getEpoch()));
      epochOfPool.setDelegators(epochRewardMap.get(epochOfPool.getEpoch()));
      epochOfPool.setFee(epochFeeMap.get(epochOfPool.getEpoch()));
      epochOfPool.setRos(0.0);//todo move sprint 3
    });
    epochRes.setData(epochOfPools);
    epochRes.setTotalItems(epochStakeProjections.getTotalElements());
    return epochRes;
  }

  @Override
  public PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String poolView) {
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    EpochChartResponse epochChart = new EpochChartResponse();
    List<EpochChartProjection> epochDataCharts = epochStakeRepository.getDataForEpochChart(poolId);
    if (!epochDataCharts.isEmpty()) {
      epochChart.setDataByDays(
          epochDataCharts.stream().map(EpochChartList::new).collect(Collectors.toList()));
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
          delegatorDataCharts.stream().map(DelegatorChartList::new).collect(Collectors.toList()));
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
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).collect(Collectors.toList());
      Set<Long> stakeAddress = delegatorList.stream()
          .map(PoolDetailDelegatorResponse::getStakeAddressId).collect(Collectors.toSet());
      List<StakeAddressProjection> stakeAddressProjections = epochStakeRepository.totalStakeByAddressAndPool(
          stakeAddress, poolId);
      Map<Long, BigInteger> stakeAddressProjectionMap = stakeAddressProjections.stream().collect(
          Collectors.toMap(StakeAddressProjection::getAddress,
              StakeAddressProjection::getTotalStake));
      delegatorList.forEach(delegator -> delegator.setTotalStake(
          stakeAddressProjectionMap.get(delegator.getStakeAddressId())));
      delegatorResponse.setTotalItems(delegatorPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
    }
    return delegatorResponse;
  }

  private BigDecimal getPoolSaturation(BigInteger reserves, Integer k) {
    if (Objects.isNull(k) || Objects.isNull(reserves)) {
      return BigDecimal.ZERO;
    }
    return (CommonConstant.TOTAL_ADA.subtract(new BigDecimal(reserves))).divide(new BigDecimal(k),
        CommonConstant.SCALE, RoundingMode.HALF_UP);
  }

  private Double getSaturation(String poolView, BigDecimal poolSaturation) {
    if (poolSaturation.equals(BigDecimal.ZERO) || Objects.isNull(poolView)) {
      return BigDecimal.ZERO.doubleValue();
    }
    Object liveStakeObject = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_PREFIX + poolView);
    if (Objects.isNull(liveStakeObject)) {
      return BigDecimal.ZERO.doubleValue();
    }
    return new BigDecimal(String.valueOf(liveStakeObject)).divide(poolSaturation,
        CommonConstant.SCALE, RoundingMode.HALF_UP).multiply(CommonConstant.PERCENT).doubleValue();
  }

  private Double getPoolRewardPercent(BigInteger activeStake, BigInteger poolReward) {
    if (Objects.isNull(activeStake) || Objects.isNull(poolReward)) {
      return BigInteger.ZERO.doubleValue();
    }
    return new BigDecimal(poolReward).divide(new BigDecimal(activeStake), CommonConstant.SCALE,
        RoundingMode.HALF_UP).multiply(CommonConstant.PERCENT).doubleValue();
  }
}
