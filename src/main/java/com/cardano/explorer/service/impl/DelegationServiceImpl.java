package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartList;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartResponse;
import com.cardano.explorer.model.response.pool.chart.EpochChartList;
import com.cardano.explorer.model.response.pool.chart.EpochChartResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;
import com.cardano.explorer.model.response.pool.projection.BasePoolChartProjection;
import com.cardano.explorer.model.response.pool.projection.DelegatorChartProjection;
import com.cardano.explorer.model.response.pool.projection.EpochChartProjection;
import com.cardano.explorer.model.response.pool.projection.EpochStakeProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailDelegatorProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.cardano.explorer.projection.PoolDelegationSummaryProjection;
import com.cardano.explorer.projection.StakeAddressProjection;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.service.DelegationService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.PoolUpdate;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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

  private final PoolOfflineDataRepository poolOfflineDataRepository;

  private final RewardRepository rewardRepository;

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
    BigDecimal totalStake = epochStakeRepository.totalStakeAllPoolByEpochNo(epochNo);
    Integer delegators = delegationRepository.numberDelegatorsAllPoolByEpochNo(
        Long.valueOf(epochNo));
    return DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(currentSlot)
        .liveStake(totalStake).delegators(delegators)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    String poolName = null;
    String poolView = null;
    if (Boolean.TRUE.equals(StringUtils.isNotBlank(search))) {
      poolView = search;
      poolName = (CommonConstant.PREFIX_POOL_NAME + search).toLowerCase();
    }
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(poolView,
        poolName, pageable);
    response.setData(poolIdPage.stream().map(
            pool -> PoolResponse.builder().poolId(pool.getPoolView())
                .poolName(getNameValueFromJson(pool.getPoolName())).poolSize(pool.getPoolSize())
                .pledge(pool.getPledge()).feeAmount(pool.getFee()).feePercent(pool.getMargin())
                .saturation(getSaturation(pool.getPoolSize(),
                    getStakeLimit(pool.getUtxo(), pool.getParamK())).doubleValue()).build())
        .collect(Collectors.toList()));
    response.setTotalItems(poolIdPage.getTotalElements());
    return response;
  }

  @Override
  public Set<PoolResponse> findTopDelegationPool(Pageable pageable) {

    if (pageable.getPageSize() > defaultSize) {
      pageable = PageRequest.of(0, defaultSize);
    }

    List<PoolDelegationSummaryProjection> pools = delegationRepository.findDelegationPoolsSummary(
        pageable);

    return pools.stream().map(pool -> {

          String poolName = getNameValueFromJson(pool.getJson());
          Integer parameterK = pool.getOptimalPoolCount();
          BigDecimal totalAda = pool.getUtxo();
          BigDecimal stakeLimit = getStakeLimit(totalAda, parameterK);
          BigDecimal poolSize = pool.getPoolSize();
          BigDecimal saturation = getSaturation(poolSize, stakeLimit);

          return PoolResponse.builder().poolId(pool.getPoolView()).poolName(poolName)
              .poolSize(pool.getPoolSize()).reward(BigInteger.ZERO.doubleValue())
              .saturation(saturation.doubleValue()).feeAmount(pool.getFee())
              .feePercent(pool.getMargin()).pledge(pool.getPledge()).build();
        }).sorted(((o1, o2) -> o2.getPoolSize().compareTo(o1.getPoolSize())))
        .collect(Collectors.toCollection(LinkedHashSet::new));

  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolView) {
    PoolDetailHeaderResponse poolDetailResponse = new PoolDetailHeaderResponse();
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    poolDetailResponse.setHashView(poolHash.getHashRaw());
    Optional<PoolOfflineData> poolOffOpt = poolOfflineDataRepository.findFirstByPoolOrderByIdDesc(
        poolHash);
    if (poolOffOpt.isPresent()) {
      poolDetailResponse.setPoolName(getNameValueFromJson(poolOffOpt.get().getJson()));
      poolDetailResponse.setTickerName(poolOffOpt.get().getTickerName());
    }
    poolDetailResponse.setPoolSize(poolHash.getPoolSize());
    Page<Timestamp> createdTime = blockRepository.getTimeCreatedPool(poolId, PageRequest.of(0, 1));
    if (!createdTime.isEmpty()) {
      poolDetailResponse.setCreateDate(createdTime.toList().get(CommonConstant.ZERO));
    }
    poolDetailResponse.setRewardAccounts(poolUpdateRepository.findRewardAccountByPool(poolId));
    poolDetailResponse.setOwnerAccounts(poolUpdateRepository.findOwnerAccountByPool(poolId));
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    List<PoolUpdate> poolUpdates = poolUpdateRepository.findAllByPoolId(poolId);
    if (Objects.nonNull(poolUpdates) && !poolUpdates.isEmpty()) {
      poolDetailResponse.setCost(poolUpdates.get(CommonConstant.ZERO).getFixedCost());
      poolDetailResponse.setMargin(poolUpdates.get(CommonConstant.ZERO).getMargin());
      poolDetailResponse.setPledge(poolUpdates.get(CommonConstant.ZERO).getPledge());
    }
    poolDetailResponse.setEpochBlock(blockRepository.getCountBlockByPoolAndCurrentEpoch(poolId));
    poolDetailResponse.setLifetimeBlock(blockRepository.getCountBlockByPool(poolId));
    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolView) {
    PoolHash poolHash = poolHashRepository.findByView(poolView)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Long poolId = poolHash.getId();
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    Page<PoolDetailEpochProjection> epochOfPoolPage = poolHashRepository.findEpochByPool(poolId,
        pageable);
    List<PoolDetailEpochResponse> epochOfPools = epochOfPoolPage.stream()
        .map(PoolDetailEpochResponse::new).collect(Collectors.toList());
    Set<Integer> epochNoInt = epochOfPoolPage.stream().map(PoolDetailEpochProjection::getEpochNo)
        .collect(Collectors.toSet());
    Set<Long> epochNoLg = epochNoInt.stream().map(Long::valueOf).collect(Collectors.toSet());
    List<EpochStakeProjection> epochStakeProjections = epochStakeRepository.totalStakeByEpochNoAndPool(
        epochNoInt, poolId);
    Map<Integer, BigDecimal> epochStakeProjectionMap = epochStakeProjections.stream().collect(
        Collectors.toMap(EpochStakeProjection::getEpochNo, EpochStakeProjection::getTotalStake));
    List<EpochStakeProjection> rewardStakeProjections = rewardRepository.totalRewardStakeByEpochNoAndPool(
        epochNoLg, poolId);
    Map<Integer, BigDecimal> rewardStakeProjectionMap = rewardStakeProjections.stream().collect(
        Collectors.toMap(EpochStakeProjection::getEpochNo, EpochStakeProjection::getTotalStake));
    List<Epoch> epochs = epochRepository.findFeeByEpochNo(epochNoInt);
    Map<Integer, BigDecimal> feeMap = epochs.stream()
        .collect(Collectors.toMap(Epoch::getNo, Epoch::getFees));
    epochOfPools.forEach(epochOfPool -> {
      epochOfPool.setStakeAmount(epochStakeProjectionMap.get(epochOfPool.getEpoch()));
      epochOfPool.setDelegators(rewardStakeProjectionMap.get(epochOfPool.getEpoch()));
      epochOfPool.setFee(feeMap.get(epochOfPool.getEpoch()));

    });

    epochRes.setData(epochOfPools);
    epochRes.setTotalItems(epochOfPoolPage.getTotalElements());
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
      Map<Long, BigDecimal> stakeAddressProjectionMap = stakeAddressProjections.stream().collect(
          Collectors.toMap(StakeAddressProjection::getAddress,
              StakeAddressProjection::getTotalStake));
      delegatorList.forEach(delegator -> delegator.setTotalStake(
          stakeAddressProjectionMap.get(delegator.getStakeAddressId())));
      delegatorResponse.setTotalItems(delegatorPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
    }
    return delegatorResponse;
  }

  /**
   * Get name value from json string pools
   *
   * @return String
   */
  private String getNameValueFromJson(String jsonName) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(jsonName))) {
      return null;
    }
    JsonObject jsonObject = new Gson().fromJson(jsonName, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }

  /**
   * get stake limit with k param
   *
   * @return BigDecimal
   */
  private BigDecimal getStakeLimit(BigDecimal totalAda, Integer k) {
    if (Objects.isNull(k) || Objects.isNull(totalAda)) {
      return BigDecimal.ZERO;
    }
    return totalAda.divide(new BigDecimal(k), CommonConstant.SCALE, RoundingMode.HALF_UP);
  }

  private static BigDecimal getSaturation(BigDecimal poolSize, BigDecimal stakeLimit) {
    if (stakeLimit.equals(BigDecimal.ZERO) || Objects.isNull(poolSize)) {
      return BigDecimal.ZERO;
    }
    return poolSize.divide(stakeLimit, CommonConstant.SCALE, RoundingMode.HALF_UP);
  }
}
