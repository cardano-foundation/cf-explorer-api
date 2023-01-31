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
      poolName = CommonConstant.PREFIX_POOL_NAME + search;
    }
    Page<PoolListProjection> poolIdPage = poolHashRepository.findAllByPoolViewAndPoolName(poolView,
        poolName, pageable);
    response.setData(
        poolIdPage.stream().map(pool -> PoolResponse.builder().poolId(pool.getPoolView())
            .poolName(getNameValueFromJson(pool.getPoolName())).poolSize(pool.getPoolSize())
            .pledge(pool.getPledge()).feeAmount(pool.getFee()).feePercent(pool.getMargin())
            .saturation(getSaturation(pool.getPoolSize(),
                getStakeLimit(pool.getUtxo(), pool.getParamK())).doubleValue())
            .reward(getReward(new RewardParam(pool)))
            .build()).collect(Collectors.toList()));
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
              .feePercent(pool.getMargin()).pledge(pool.getPledge())
              .reward(getReward(new RewardParam(pool)))
              .build();
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
    List<Timestamp> createdTime = blockRepository.getTimeCreatedPool(poolId);
    if (!createdTime.isEmpty()) {
      poolDetailResponse.setCreateDate(createdTime.get(CommonConstant.ZERO));
    }
    poolDetailResponse.setRewardAccounts(poolUpdateRepository.findRewardAccountByPool(poolId));
    poolDetailResponse.setOwnerAccounts(poolUpdateRepository.findOwnerAccountByPool(poolId));
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    poolDetailResponse.setReward(CommonConstant.ZERO.doubleValue());
    poolDetailResponse.setStakeLimit(
        getStakeLimit(poolDetailProjection.getUtxo(), poolDetailProjection.getParamK()));
    poolDetailResponse.setSaturation(getSaturation(poolDetailProjection.getPoolSize(),
        poolDetailResponse.getStakeLimit()).doubleValue());
    Double reward = getReward(new RewardParam(poolDetailProjection));
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
        RewardParam param = new RewardParam(rewardEpochProjection);
        param.setMargin(poolUpdate.getMargin());
        param.setPledge(poolUpdate.getPledge());
        param.setFixedFee(poolUpdate.getFixedCost());
        param.setPoolSize(poolHash.getPoolSize());
        Double reward = getReward(param);
        epochOfPool.setRos(getPoolRos(reward, poolUpdate.getMargin()));
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
  private BigDecimal getTotalADAInCirculation(BigDecimal currentAda, Double expansionRate) {
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(currentAda);
    return currentAda.add(
        (reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
  }

  /**
   * get R param
   *
   * @return BigDecimal
   */
  private BigDecimal getParamR(BigDecimal currentAda, Double expansionRate, BigDecimal feePerEpoch,
      Double treasuryRate) {
    if (Objects.isNull(currentAda) || Objects.isNull(expansionRate) || Objects.isNull(
        treasuryRate)) {
      return BigDecimal.ZERO;
    }
    if (Objects.isNull(feePerEpoch) || feePerEpoch.compareTo(BigDecimal.ZERO) < 0) {
      feePerEpoch = BigDecimal.ZERO;
    }
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(currentAda);
    BigDecimal totalADAInCirculation = currentAda.add(
        (reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
    BigDecimal reserveTwo = CommonConstant.TOTAL_ADA.subtract(totalADAInCirculation);
    return ((reserveTwo.multiply(BigDecimal.valueOf(expansionRate))).add(feePerEpoch)).multiply(
        BigDecimal.ONE.subtract(BigDecimal.valueOf(treasuryRate)));
  }

  /**
   * get gross reward for pool
   *
   * @return BigDecimal
   */
  private BigDecimal getGrossReward(Integer k, BigDecimal pledge, BigDecimal totalADAInCirculation,
      Double a0,
      BigDecimal poolSize, BigDecimal r) {
    if (r.equals(BigDecimal.ZERO) || Objects.isNull(pledge) || Objects.isNull(k) || Objects.isNull(
        a0) || Objects.isNull(poolSize) || poolSize.equals(BigDecimal.ZERO)) {
      return BigDecimal.ZERO;
    }
    BigDecimal z0 = BigDecimal.ONE.divide(BigDecimal.valueOf(k), CommonConstant.SCALE_10,
        RoundingMode.HALF_DOWN);
    BigDecimal s = pledge.divide(totalADAInCirculation, CommonConstant.SCALE_10,
        RoundingMode.HALF_DOWN);
    BigDecimal sigma = poolSize.divide(totalADAInCirculation, CommonConstant.SCALE_10,
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
      Double margin, BigDecimal fixedFee) {
    if (grossReward.equals(BigDecimal.ZERO) || Objects.isNull(blkCount) || Objects.isNull(
        maxBlockSize) || Objects.isNull(margin) || Objects.isNull(fixedFee)) {
      return BigDecimal.ZERO;
    }
    BigDecimal stakePoolPerformance = BigDecimal.valueOf(blkCount)
        .divide(BigDecimal.valueOf(maxBlockSize), CommonConstant.SCALE_10, RoundingMode.HALF_DOWN);
    BigDecimal penalty = BigDecimal.ONE.subtract(stakePoolPerformance).multiply(grossReward);
    grossReward = BigDecimal.ZERO.max(grossReward.subtract(penalty));
    grossReward = BigDecimal.ZERO.max(
        grossReward.subtract((fixedFee.multiply(CommonConstant.DAY_IN_EPOCH))));
    BigDecimal marginAda = grossReward.multiply(BigDecimal.valueOf(margin));
    return grossReward.subtract(marginAda);
  }

  /**
   * get reward ada for pool calculate capped ada
   *
   * @return BigDecimal
   */
  private Double getReward(BigDecimal poolSize, BigDecimal pledge, Integer k, BigDecimal currentAda,
      Double expansionRate, BigDecimal netReward) {
    if (netReward.equals(BigDecimal.ZERO)) {
      return BigDecimal.ZERO.doubleValue();
    }
    BigDecimal ada = poolSize.subtract(pledge);
    BigDecimal reserveOne = CommonConstant.TOTAL_ADA.subtract(currentAda);
    BigDecimal totalADAInCirculation = currentAda.add(
        (reserveOne.multiply(BigDecimal.valueOf(expansionRate))));
    BigDecimal poolSaturation = totalADAInCirculation.divide(BigDecimal.valueOf(k),
        CommonConstant.SCALE_10, RoundingMode.HALF_DOWN);
    BigDecimal sigma = poolSize.divide(currentAda, CommonConstant.SCALE_10, RoundingMode.HALF_DOWN);
    BigDecimal control = sigma.multiply(totalADAInCirculation);
    BigDecimal cappedAda = ada.min(
        BigDecimal.ZERO.max(poolSaturation.min(control).subtract(poolSize)));
    BigDecimal stakePoolControlADA = sigma.multiply(totalADAInCirculation);
    BigDecimal rewardAda = ((netReward.multiply(cappedAda)).divide(stakePoolControlADA,
        CommonConstant.SCALE_10, RoundingMode.HALF_DOWN)).multiply(CommonConstant.EPOCH_IN_YEARS);
    return rewardAda.divide(ada, CommonConstant.SCALE_10, RoundingMode.HALF_DOWN).doubleValue();
  }

  private Double getReward(RewardParam param) {
    BigDecimal totalADAInCirculation = getTotalADAInCirculation(param.getCurrentAda(),
        param.getExpansionRate());
    BigDecimal r = getParamR(param.getCurrentAda(),
        param.getExpansionRate(), param.getFeePerEpoch(),
        param.getTreasuryRate());
    BigDecimal grossReward = getGrossReward(param.getK(),
        param.getPledge(), totalADAInCirculation,
        param.getA0(), param.getPoolSize(), r);
    BigDecimal netReward = getNetReward(grossReward, param.getBlkCount(),
        param.getMaxBlockSize(), param.getMargin(),
        param.getFixedFee());
    return getReward(param.getPoolSize(), param.getPledge(),
        param.getK(), param.getCurrentAda(),
        param.getExpansionRate(), netReward);
  }
}
