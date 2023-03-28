package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.model.request.pool.RewardParam;
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
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.service.DelegationService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolUpdate;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
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
    BigInteger totalStake = epochStakeRepository.totalStakeAllPoolByEpochNo(epochNo)
        .orElse(BigInteger.ZERO);
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
      poolName = CommonConstant.PREFIX_POOL_NAME + search;
    }
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(poolView,
        poolName, pageable);
    List<PoolResponse> poolList = new ArrayList<>();
    List<String> poolViews = new ArrayList<>();
    poolIdPage.stream().forEach(pool -> {
      poolList.add(PoolResponse.builder().poolId(pool.getPoolView())
          .poolName(getNameValueFromJson(pool.getPoolName()))
          .poolSize(pool.getPoolSize())
          .pledge(pool.getPledge())
          .feeAmount(pool.getFee())
          .feePercent(pool.getMargin())
          .saturation(getSaturation(pool.getPoolView(),
              getPoolSaturation(pool.getReserves(), pool.getParamK())).doubleValue())
          .build());
      poolViews.add(pool.getPoolView());
    });
    List<PoolListProjection> dataRewards = poolHashRepository.findDataCalculateReward(poolViews);
    Map<String, PoolListProjection> dataRewardsMap = dataRewards.stream()
        .collect(Collectors.toMap(PoolListProjection::getPoolView, Function.identity()));
    poolList.forEach(
        pool -> pool.setReward(getReward(new RewardParam(dataRewardsMap.get(pool.getPoolId())))));
    response.setData(poolList);
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
    List<String> poolViews = pools.stream().map(PoolDelegationSummaryProjection::getPoolView)
        .collect(
            Collectors.toList());
    List<PoolListProjection> dataRewards = poolHashRepository.findDataCalculateReward(poolViews);
    Map<String, PoolListProjection> dataRewardsMap = dataRewards.stream()
        .collect(Collectors.toMap(PoolListProjection::getPoolView, Function.identity()));
    return pools.stream().map(pool -> {

          String poolName = getNameValueFromJson(pool.getJson());
          Integer parameterK = pool.getOptimalPoolCount();
          BigDecimal saturation = getSaturation(pool.getPoolView(),
              getPoolSaturation(pool.getReserves(), parameterK));
          return PoolResponse.builder().poolId(pool.getPoolView()).poolName(poolName)
              .poolSize(pool.getPoolSize()).reward(BigInteger.ZERO.doubleValue())
              .saturation(saturation.doubleValue()).feeAmount(pool.getFee())
              .feePercent(pool.getMargin()).pledge(pool.getPledge())
              .reward(getReward(new RewardParam(dataRewardsMap.get(pool.getPoolView())))).build();
        }).sorted(((o1, o2) -> o2.getPoolSize().compareTo(o1.getPoolSize())))
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolView) {
    PoolDetailUpdateProjection poolDetailProjection = poolHashRepository.getDataForPoolDetail(
        poolView);
    Long poolId = poolDetailProjection.getPoolId();
    PoolDetailHeaderResponse poolDetailResponse = Stream.of(poolDetailProjection)
        .map(PoolDetailHeaderResponse::new).findFirst()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolName(getNameValueFromJson(poolDetailResponse.getPoolName()));
    poolDetailResponse.setCreateDate(poolUpdateRepository.getCreatedTimeOfPool(poolId));
    poolDetailResponse.setRewardAccounts(poolUpdateRepository.findRewardAccountByPool(poolId));
    List<String> ownerAddress = poolUpdateRepository.findOwnerAccountByPool(poolId);
    Collections.sort(ownerAddress);
    poolDetailResponse.setOwnerAccounts(ownerAddress);
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    poolDetailResponse.setReward(CommonConstant.ZERO.doubleValue());
    poolDetailResponse.setSaturation(getSaturation(poolView,
        getPoolSaturation(poolDetailProjection.getReserves(),
            poolDetailProjection.getParamK())).doubleValue());
    PoolListProjection poolListProjection = poolHashRepository.findDataCalculateReward(poolView);
    poolDetailResponse.setStakeLimit(
        getStakeLimit(poolListProjection.getUtxo(), poolDetailProjection.getParamK()));
    Double reward = getReward(new RewardParam(poolListProjection));
    poolDetailResponse.setReward(reward);
    poolDetailResponse.setRos(getPoolRos(reward, poolDetailProjection.getMargin()));
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
    List<EpochStakeProjection> epochStakeProjections = epochStakeRepository.totalStakeByEpochNoAndPool(
        epochNoInt, poolId);
    Map<Integer, BigInteger> epochStakeProjectionMap = epochStakeProjections.stream().collect(
        Collectors.toMap(EpochStakeProjection::getEpochNo, EpochStakeProjection::getTotalStake));
    List<Epoch> epochList = epochRepository.findFeeByEpochNo(epochNoInt);
    Map<Integer, BigInteger> feesMap = epochList.stream().collect(Collectors.toMap(Epoch::getNo, Epoch::getFees));
    List<RewardEpochProjection> rewardEpochProjections = epochRepository.findParamRewardByEpoch(
        epochNoInt);
    Map<Integer, RewardEpochProjection> rewardEpochProjectionMap = rewardEpochProjections.stream()
        .collect(Collectors.toMap(RewardEpochProjection::getEpochNo, Function.identity()));
    epochOfPools.forEach(epochOfPool -> {
      epochOfPool.setFee(feesMap.get(epochOfPool.getEpoch()));
      epochOfPool.setStakeAmount(epochStakeProjectionMap.get(epochOfPool.getEpoch()));
      RewardEpochProjection rewardEpochProjection = rewardEpochProjectionMap.get(
          epochOfPool.getEpoch());
      if (Objects.nonNull(rewardEpochProjection) && Objects.nonNull(poolUpdate)) {
        RewardParam param = new RewardParam(rewardEpochProjection);
        param.setMargin(poolUpdate.getMargin());
        param.setPledge(poolUpdate.getPledge());
        param.setFixedFee(poolUpdate.getFixedCost());
        param.setPoolSize(poolHash.getPoolSize());
        Double reward = getReward(param);
        //Todo add delegators reward
        epochOfPool.setRos(getPoolRos(reward, poolUpdate.getMargin()));
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

  /**
   * Get name value from json string pools
   *
   * @return String
   */
  private String getNameValueFromJson(String jsonName) {
    try {
      JsonObject jsonObject = new Gson().fromJson(jsonName, JsonObject.class);
      return jsonObject.get("name").getAsString();
    } catch (Exception ex) {
      log.error("Error: when convert json string to json object");
    }
    return null;
  }

  /**
   * get stake limit with k param
   *
   * @return BigDecimal
   */
  private BigDecimal getStakeLimit(BigInteger totalAda, Integer k) {
    if (Objects.isNull(k) || Objects.isNull(totalAda)) {
      return BigDecimal.ZERO;
    }
    return new BigDecimal(totalAda).divide(new BigDecimal(k), CommonConstant.SCALE,
        RoundingMode.HALF_UP);
  }

  private BigDecimal getPoolSaturation(BigInteger reserves, Integer k) {
    if (Objects.isNull(k) || Objects.isNull(reserves)) {
      return BigDecimal.ZERO;
    }
    return (CommonConstant.TOTAL_ADA.subtract(new BigDecimal(reserves))).divide(new BigDecimal(k),
        CommonConstant.SCALE, RoundingMode.HALF_UP);
  }

  private BigDecimal getSaturation(String poolView, BigDecimal poolSaturation) {
    if (poolSaturation.equals(BigDecimal.ZERO) || Objects.isNull(poolView)) {
      return BigDecimal.ZERO;
    }
    Object liveStakeObject = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_PREFIX + poolView);
    if (Objects.isNull(liveStakeObject)) {
      return BigDecimal.ZERO;
    }
    return new BigDecimal(String.valueOf(liveStakeObject)).divide(poolSaturation,
        CommonConstant.SCALE, RoundingMode.HALF_UP);
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

  /**
   * get totalADAInCirculation param
   *
   * @return BigDecimal
   */
  private BigDecimal getTotalADAInCirculation(BigInteger currentAda, Double expansionRate) {
    if (Objects.isNull(currentAda) || Objects.isNull(expansionRate)) {
      return BigDecimal.ZERO;
    }
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(new BigDecimal(currentAda));
    return new BigDecimal(currentAda).add((reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
  }

  /**
   * get R param
   *
   * @return BigDecimal
   */
  private BigDecimal getParamR(BigInteger currentAda, Double expansionRate, BigInteger feePerEpoch,
      Double treasuryRate) {
    if (Objects.isNull(currentAda) || Objects.isNull(expansionRate) || Objects.isNull(
        treasuryRate)) {
      return BigDecimal.ZERO;
    }
    if (Objects.isNull(feePerEpoch) || feePerEpoch.compareTo(BigInteger.ZERO) < 0) {
      feePerEpoch = BigInteger.ZERO;
    }
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(new BigDecimal(currentAda));
    BigDecimal totalADAInCirculation = new BigDecimal(currentAda).add(
        (reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
    BigDecimal reserveTwo = CommonConstant.TOTAL_ADA.subtract(totalADAInCirculation);
    return ((reserveTwo.multiply(BigDecimal.valueOf(expansionRate))).add(
        new BigDecimal(feePerEpoch))).multiply(
        BigDecimal.ONE.subtract(BigDecimal.valueOf(treasuryRate)));
  }

  /**
   * get gross reward for pool
   *
   * @return BigDecimal
   */
  private BigDecimal getGrossReward(Integer k, BigInteger currentAda,
      BigDecimal totalADAInCirculation,
      Double a0, BigInteger poolSize, BigDecimal r) {
    if (r.equals(BigDecimal.ZERO) || Objects.isNull(k) || Objects.isNull(
        a0) || Objects.isNull(poolSize) || poolSize.equals(BigDecimal.ZERO)) {
      return BigDecimal.ZERO;
    }
    BigDecimal z0 = BigDecimal.ONE.divide(BigDecimal.valueOf(k), CommonConstant.SCALE,
        RoundingMode.HALF_DOWN);
    BigDecimal poolSaturation = totalADAInCirculation.divide(BigDecimal.valueOf(k),
        CommonConstant.SCALE, RoundingMode.HALF_DOWN);
    BigDecimal s = (new BigDecimal(poolSize).min(poolSaturation)).divide(totalADAInCirculation,
        CommonConstant.SCALE, RoundingMode.HALF_DOWN);
    BigDecimal sigma = new BigDecimal(poolSize).divide(new BigDecimal(currentAda),
        CommonConstant.SCALE,
        RoundingMode.HALF_DOWN);
    BigDecimal sCapped = z0.min(s);
    BigDecimal sigmaCapped = z0.min(sigma);
    return (r.divide(BigDecimal.ONE.add(BigDecimal.valueOf(a0)), CommonConstant.SCALE,
        RoundingMode.HALF_DOWN)).multiply(sigmaCapped.add(sCapped.multiply(BigDecimal.valueOf(a0))
        .multiply(((sigmaCapped.subtract(sCapped)).multiply(
            (z0.subtract(sigmaCapped)).divide(z0, CommonConstant.SCALE,
                RoundingMode.HALF_DOWN))).divide(z0, CommonConstant.SCALE,
            RoundingMode.HALF_DOWN))));
  }

  /**
   * get net reward for pool
   *
   * @return BigDecimal
   */
  private BigDecimal getNetReward(BigDecimal grossReward, Integer blkCount, Integer maxBlockSize,
      Double margin, BigInteger fixedFee) {
    if (grossReward.equals(BigDecimal.ZERO) || Objects.isNull(blkCount) || Objects.isNull(
        maxBlockSize) || Objects.isNull(margin) || Objects.isNull(fixedFee)) {
      return BigDecimal.ZERO;
    }
    BigDecimal stakePoolPerformance = BigDecimal.valueOf(blkCount)
        .divide(BigDecimal.valueOf(maxBlockSize), CommonConstant.SCALE, RoundingMode.HALF_DOWN);
    BigDecimal penalty = BigDecimal.ONE.subtract(stakePoolPerformance).multiply(grossReward);
    grossReward = BigDecimal.ZERO.max(grossReward.subtract(penalty));
    grossReward = BigDecimal.ZERO.max(grossReward.subtract(new BigDecimal(fixedFee)));
    BigDecimal marginAda = grossReward.multiply(BigDecimal.valueOf(margin));
    return grossReward.subtract(marginAda);
  }

  /**
   * get reward ada for pool calculate capped ada
   *
   * @return BigDecimal
   */
  private Double getReward(BigInteger poolSize, BigInteger pledge, Integer k, BigInteger currentAda,
      Double expansionRate, BigDecimal netReward) {
    if (netReward.equals(BigDecimal.ZERO) || poolSize.compareTo(pledge) <= 0) {
      return BigDecimal.ZERO.doubleValue();
    }
    BigDecimal ada = new BigDecimal(poolSize).subtract(new BigDecimal(pledge));
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(new BigDecimal(currentAda));
    BigDecimal totalADAInCirculation = new BigDecimal(currentAda).add(
        (reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
    BigDecimal poolSaturation = totalADAInCirculation.divide(BigDecimal.valueOf(k),
        CommonConstant.SCALE, RoundingMode.HALF_DOWN);
    BigDecimal sigma = new BigDecimal(poolSize).divide(new BigDecimal(currentAda),
        CommonConstant.SCALE, RoundingMode.HALF_DOWN);
    BigDecimal control = sigma.multiply(totalADAInCirculation);
    BigDecimal cappedAda = ada.min(
        BigDecimal.ZERO.max(poolSaturation.min(control).subtract(new BigDecimal(poolSize))));
    BigDecimal stakePoolControlADA = sigma.multiply(totalADAInCirculation);
    BigDecimal rewardAda = ((netReward.multiply(cappedAda)).divide(stakePoolControlADA,
        CommonConstant.SCALE, RoundingMode.HALF_DOWN)).multiply(CommonConstant.EPOCH_IN_YEARS);
    return rewardAda.divide(ada, CommonConstant.SCALE, RoundingMode.HALF_DOWN).doubleValue();
  }

  private Double getReward(RewardParam param) {
    BigDecimal totalADAInCirculation = getTotalADAInCirculation(param.getCurrentAda(),
        param.getExpansionRate());
    BigDecimal r = getParamR(param.getCurrentAda(), param.getExpansionRate(),
        param.getFeePerEpoch(), param.getTreasuryRate());
    BigDecimal grossReward = getGrossReward(param.getK(), param.getCurrentAda(),
        totalADAInCirculation, param.getA0(),
        param.getPoolSize(), r);
    BigDecimal netReward = getNetReward(grossReward, param.getBlkCount(), param.getMaxBlockSize(),
        param.getMargin(),
        param.getFixedFee());
    return getReward(
        param.getPoolSize(),
        param.getPledge(),
        param.getK(),
        param.getCurrentAda(),
        param.getExpansionRate(), netReward);
  }
}
