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
import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.cardano.explorer.model.response.pool.projection.RewardEpochProjection;
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
import java.util.function.Function;
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
    response.setData(poolIdPage.stream().map(pool -> {
      BigDecimal poolReward = getPoolReward(pool.getUtxo(), pool.getExpansionRate(),
          pool.getFeePerEpoch(), pool.getTreasuryRate());
      BigDecimal poolRewardInEpoch = getPoolRewardInEpoch(pool.getParamK(), pool.getPledge(),
          pool.getUtxo(), pool.getInfluence(), pool.getPoolSize(), poolReward);
      Double annualizedPoolReward = getAnnualizedPoolReward(poolRewardInEpoch, pool.getPoolSize());
      return PoolResponse.builder().poolId(pool.getPoolView())
          .poolName(getNameValueFromJson(pool.getPoolName())).poolSize(pool.getPoolSize())
          .pledge(pool.getPledge()).feeAmount(pool.getFee()).feePercent(pool.getMargin())
          .saturation(getSaturation(pool.getPoolSize(),
              getStakeLimit(pool.getUtxo(), pool.getParamK())).doubleValue())
          .reward(annualizedPoolReward).build();
    }).collect(Collectors.toList()));
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
          BigDecimal poolReward = getPoolReward(pool.getUtxo(), pool.getExpansionRate(),
              pool.getFeePerEpoch(), pool.getTreasuryRate());
          BigDecimal poolRewardInEpoch = getPoolRewardInEpoch(pool.getOptimalPoolCount(),
              pool.getPledge(), pool.getUtxo(), pool.getInfluence(), pool.getPoolSize(), poolReward);
          Double annualizedPoolReward = getAnnualizedPoolReward(poolRewardInEpoch, pool.getPoolSize());
          return PoolResponse.builder().poolId(pool.getPoolView()).poolName(poolName)
              .poolSize(pool.getPoolSize()).reward(BigInteger.ZERO.doubleValue())
              .saturation(saturation.doubleValue()).feeAmount(pool.getFee())
              .feePercent(pool.getMargin()).pledge(pool.getPledge()).reward(annualizedPoolReward)
              .build();
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
    poolDetailResponse.setReward(CommonConstant.ZERO.doubleValue());
    PoolDetailUpdateProjection poolUpdates = poolUpdateRepository.findPoolUpdateByPoolId(poolId);
    if (Objects.nonNull(poolUpdates)) {
      poolDetailResponse.setCost(poolUpdates.getCost());
      poolDetailResponse.setMargin(poolUpdates.getMargin());
      poolDetailResponse.setPledge(poolUpdates.getPledge());
      poolDetailResponse.setStakeLimit(
          getStakeLimit(poolUpdates.getUtxo(), poolUpdates.getParamK()));
      poolDetailResponse.setSaturation(
          getSaturation(poolHash.getPoolSize(), poolDetailResponse.getStakeLimit()).doubleValue());
      BigDecimal poolReward = getPoolReward(poolUpdates.getUtxo(), poolUpdates.getExpansionRate(),
          poolUpdates.getFeePerEpoch(), poolUpdates.getTreasuryRate());
      BigDecimal poolRewardInEpoch = getPoolRewardInEpoch(poolUpdates.getParamK(),
          poolUpdates.getPledge(), poolUpdates.getUtxo(), poolUpdates.getInfluence(),
          poolHash.getPoolSize(), poolReward);
      Double annualizedPoolReward = getAnnualizedPoolReward(poolRewardInEpoch,
          poolHash.getPoolSize());
      poolDetailResponse.setReward(annualizedPoolReward);
      poolDetailResponse.setRos(getPoolRos(annualizedPoolReward, poolUpdates.getMargin()));
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
    PoolUpdate poolUpdate = poolUpdateRepository.findLastEpochByPool(poolId);
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
    List<RewardEpochProjection> rewardEpochProjections = epochRepository.findParamRewardByEpoch(
        epochNoInt);
    Map<Integer, RewardEpochProjection> rewardEpochProjectionMap = rewardEpochProjections.stream()
        .collect(Collectors.toMap(RewardEpochProjection::getEpochNo, Function.identity()));
    epochOfPools.forEach(epochOfPool -> {
      epochOfPool.setStakeAmount(epochStakeProjectionMap.get(epochOfPool.getEpoch()));
      epochOfPool.setDelegators(rewardStakeProjectionMap.get(epochOfPool.getEpoch()));
      RewardEpochProjection rewardEpochProjection = rewardEpochProjectionMap.get(
          epochOfPool.getEpoch());
      if (Objects.nonNull(rewardEpochProjection) && Objects.nonNull(poolUpdate)) {
        BigDecimal poolReward = getPoolReward(rewardEpochProjection.getUtxo(),
            rewardEpochProjection.getExpansionRate(), rewardEpochProjection.getFeePerEpoch(),
            rewardEpochProjection.getTreasuryRate());
        BigDecimal poolRewardInEpoch = getPoolRewardInEpoch(rewardEpochProjection.getParamK(),
            poolUpdate.getPledge(), rewardEpochProjection.getUtxo(),
            rewardEpochProjection.getInfluence(), poolHash.getPoolSize(), poolReward);
        Double annualizedPoolReward = getAnnualizedPoolReward(poolRewardInEpoch,
            poolHash.getPoolSize());
        epochOfPool.setRos(getPoolRos(annualizedPoolReward, poolUpdate.getMargin()));
        epochOfPool.setFee(rewardEpochProjection.getFeePerEpoch());
      }
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

  /**
   * get pool reward for pool
   *
   * @return BigDecimal
   */
  private BigDecimal getPoolReward(BigDecimal currentAda, Double expansionRate,
      BigDecimal feePerEpoch, Double treasuryRate) {
    if (Objects.isNull(currentAda) || Objects.isNull(expansionRate) || Objects.isNull(feePerEpoch)
        || Objects.isNull(treasuryRate)) {
      return BigDecimal.ZERO;
    }
    BigDecimal grossReward = CommonConstant.TOTAL_ADA.subtract(currentAda)
        .multiply(BigDecimal.valueOf(expansionRate)).add(feePerEpoch);
    return grossReward.subtract(grossReward.multiply(BigDecimal.valueOf(treasuryRate)));
  }

  /**
   * get pool reward in epoch for pool
   *
   * @return BigDecimal
   */
  private BigDecimal getPoolRewardInEpoch(Integer k, BigDecimal pledge, BigDecimal currentAda,
      Double a0, BigDecimal poolSize, BigDecimal poolReward) {
    if (Objects.isNull(k) || Objects.isNull(currentAda) || Objects.isNull(a0) || Objects.isNull(
        pledge) || Objects.isNull(poolSize)) {
      return BigDecimal.ZERO;
    }
    BigDecimal z0 = BigDecimal.ONE.divide(BigDecimal.valueOf(k), CommonConstant.SCALE,
        RoundingMode.HALF_DOWN);
    BigDecimal s = pledge.divide(currentAda, CommonConstant.SCALE_10, RoundingMode.HALF_DOWN);
    BigDecimal sigma = poolSize.divide(currentAda, CommonConstant.SCALE_10, RoundingMode.HALF_DOWN);
    BigDecimal sCapped = z0.min(s);
    BigDecimal sigmaCapped = z0.min(sigma);
    return poolReward.divide(BigDecimal.ONE.add(BigDecimal.valueOf(a0)), CommonConstant.SCALE,
        RoundingMode.HALF_DOWN).multiply(sigmaCapped.add(sCapped.multiply(BigDecimal.valueOf(a0)
        .multiply((sigmaCapped.subtract(sCapped.multiply(
            (z0.subtract(sigmaCapped)).divide(z0, CommonConstant.SCALE,
                RoundingMode.HALF_DOWN)))).divide(z0, CommonConstant.SCALE,
            RoundingMode.HALF_DOWN)))));
  }

  /**
   * get annualized pool reward for pool
   *
   * @return Double
   */
  private Double getAnnualizedPoolReward(BigDecimal poolRewardInEpoch, BigDecimal poolSize) {
    if (Objects.isNull(poolSize) || poolSize.equals(BigDecimal.ZERO)
        || poolRewardInEpoch.compareTo(BigDecimal.ZERO) < 0) {
      return CommonConstant.ZERO.doubleValue();
    }
    return poolRewardInEpoch.multiply(CommonConstant.EPOCH_IN_YEARS)
        .divide(poolSize, CommonConstant.SCALE, RoundingMode.HALF_DOWN).doubleValue();
  }

  /**
   * get ROS for pool
   *
   * @return Double
   */
  private Double getPoolRos(Double reward, Double margin) {
    if (Objects.isNull(margin)) {
      return CommonConstant.ZERO.doubleValue();
    }
    return BigDecimal.valueOf(reward)
        .subtract(BigDecimal.valueOf(reward).multiply(BigDecimal.valueOf(margin))).doubleValue();
  }
}
