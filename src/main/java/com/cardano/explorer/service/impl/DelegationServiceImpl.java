package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartList;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartResponse;
import com.cardano.explorer.model.response.pool.chart.EpochChartList;
import com.cardano.explorer.model.response.pool.chart.EpochChartResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;
import com.cardano.explorer.model.response.pool.projection.BasePoolChartProjection;
import com.cardano.explorer.model.response.pool.projection.DelegatorChartProjection;
import com.cardano.explorer.model.response.pool.projection.EpochChartProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailDelegatorProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.TxBlockEpochProjection;
import com.cardano.explorer.model.response.pool.projection.TxPoolProjection;
import com.cardano.explorer.projection.PoolDelegationSummaryProjection;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
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
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
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

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;

  private final RewardRepository rewardRepository;

  private static final Integer ZERO = 0;
  private static final Integer DEFAULT_EPOCH_STAKE = 20;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;

  private static final String PREFIX_POOL_NAME = "{\"name\": \"";

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
        .countDownEndTime(countDownTime > ZERO ? countDownTime : ZERO).build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    Page<Long> poolIdPage;
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(search))) {
      poolIdPage = poolHashRepository.findAllByPoolId(null, pageable);
    } else {
      if (Boolean.TRUE.equals(StringUtils.isNumeric(search))) {
        poolIdPage = poolHashRepository.findAllByPoolId(Long.valueOf(search), pageable);
      } else {
        poolIdPage = poolHashRepository.findAllByPoolName((PREFIX_POOL_NAME + search).toLowerCase(),
            pageable);
      }
    }
    if (Objects.isNull(poolIdPage)) {
      log.warn("Pool list is empty");
      return response;
    }
    List<PoolResponse> pools = poolIdPage.stream().map(PoolResponse::new)
        .collect(Collectors.toList());
    pools.forEach(pool -> {
      List<BigDecimal> pledges = poolUpdateRepository.findPledgeByPool(pool.getPoolId());
      if (Objects.nonNull(pledges) && !pledges.isEmpty()) {
        pool.setPledge(pledges.get(ZERO));
      }
      List<String> poolNames = poolOfflineDataRepository.findAllByPool(pool.getPoolId());
      if (Objects.nonNull(poolNames) && !poolNames.isEmpty()) {
        pool.setPoolName(getNameValueFromJson(poolNames.get(ZERO)));
      }
      pool.setPoolSize(poolHashRepository.getPoolSizeByPoolId(pool.getPoolId()));
    });
    response.setTotalItems(poolIdPage.getTotalElements());
    response.setData(pools);
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

          return PoolResponse.builder()
              .poolId(pool.getPoolId())
              .poolName(poolName)
              .poolSize(pool.getPoolSize())
              .reward(BigInteger.ZERO.doubleValue())
              .saturation(BigInteger.ZERO.doubleValue())
              .feeAmount(pool.getFee())
              .feePercent(BigInteger.ZERO.doubleValue())
              .pledge(pool.getPledge())
              .build();
        })
        .sorted(((o1, o2) -> o2.getPoolSize().compareTo(o1.getPoolSize())))
        .collect(Collectors.toCollection(LinkedHashSet::new));

  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(Long poolId) {
    PoolDetailHeaderResponse poolDetailResponse = new PoolDetailHeaderResponse();
    PoolHash poolHash = poolHashRepository.findById(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolView(poolHash.getHashRaw());
    PoolOfflineData poolOff = poolOfflineDataRepository.findFirstByPool(poolHash)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolName(getNameValueFromJson(poolOff.getJson()));
    poolDetailResponse.setTickerName(poolOff.getTickerName());
    poolDetailResponse.setPoolSize(poolHash.getPoolSize());
    Page<Timestamp> createdTime = blockRepository.getTimeCreatedPool(poolId, PageRequest.of(0, 1));
    if (!createdTime.isEmpty()) {
      poolDetailResponse.setCreateDate(createdTime.toList().get(ZERO));
    }
    poolDetailResponse.setRewardAccounts(poolUpdateRepository.findRewardAccountByPool(poolId));
    poolDetailResponse.setOwnerAccounts(poolUpdateRepository.findOwnerAccountByPool(poolId));
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    List<PoolUpdate> poolUpdates = poolUpdateRepository.findAllByPoolId(poolId);
    if (Objects.nonNull(poolUpdates) && !poolUpdates.isEmpty()) {
      poolDetailResponse.setCost(poolUpdates.get(ZERO).getFixedCost());
      poolDetailResponse.setMargin(poolUpdates.get(ZERO).getMargin());
      poolDetailResponse.setPledge(poolUpdates.get(ZERO).getPledge());
    }
    poolDetailResponse.setEpochBlock(blockRepository.getCountBlockByPoolAndCurrentEpoch(poolId));
    poolDetailResponse.setLifetimeBlock(blockRepository.getCountBlockByPool(poolId));
    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      Long poolId) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    Page<PoolDetailEpochProjection> epochOfPoolPage = poolHashRepository.findEpochByPool(poolId,
        pageable);
    List<PoolDetailEpochResponse> epochOfPools = epochOfPoolPage.stream()
        .map(PoolDetailEpochResponse::new).collect(Collectors.toList());
    epochOfPools.forEach(epochOfPool -> {
      epochOfPool.setStakeAmount(
          epochStakeRepository.totalStakeByEpochNoAndPool(epochOfPool.getEpoch(), poolId));
      epochOfPool.setDelegators(
          rewardRepository.totalRewardStakeByEpochNoAndPool(Long.valueOf(epochOfPool.getEpoch()),
              poolId));
      epochOfPool.setFee(epochRepository.findFeeByEpochNo(epochOfPool.getEpoch()));
    });
    epochRes.setData(epochOfPools);
    epochRes.setTotalItems(epochOfPoolPage.getTotalElements());
    return epochRes;
  }

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage = stakeRegistrationRepository.getDataForPoolRegistration(
        pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    poolTxRes.forEach(poolTx -> {
      List<TxPoolProjection> trxPools = poolHashRepository.getDataForPoolTx(poolTx.getBlock());
      if (Objects.nonNull(trxPools) && !trxPools.isEmpty()) {
        TxPoolProjection trxPool = trxPools.get(ZERO);
        poolTx.setPledge(trxPool.getPledge());
        poolTx.setCost(trxPool.getCost());
        poolTx.setMargin(trxPool.getMargin());
        List<String> poolNames = poolOfflineDataRepository.findAllByPool(trxPool.getPoolId());
        if (Objects.nonNull(poolNames) && !poolNames.isEmpty()) {
          poolTx.setPoolName(getNameValueFromJson(poolNames.get(ZERO)));
        }
      }
    });
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return response;
  }

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolDeRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage = stakeDeRegistrationRepository.getDataForPoolDeRegistration(
        pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    poolTxRes.forEach(poolTx -> {
      List<TxPoolProjection> trxPools = poolHashRepository.getDataForPoolTx(poolTx.getBlock());
      if (Objects.nonNull(trxPools) && !trxPools.isEmpty()) {
        TxPoolProjection trxPool = trxPools.get(ZERO);
        poolTx.setPledge(trxPool.getPledge());
        poolTx.setCost(trxPool.getCost());
        poolTx.setMargin(trxPool.getMargin());
        List<String> poolNames = poolOfflineDataRepository.findAllByPool(trxPool.getPoolId());
        if (Objects.nonNull(poolNames) && !poolNames.isEmpty()) {
          poolTx.setPoolName(getNameValueFromJson(poolNames.get(0)));
        }
      }
    });
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return response;
  }

  @Override
  public PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(Long poolId) {
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
      Pageable pageable, Long poolId) {
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    Page<PoolDetailDelegatorProjection> delegatorPage = delegationRepository.getAllDelegatorByPool(
        poolId, pageable);
    if (!delegatorPage.isEmpty()) {
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).collect(Collectors.toList());
      delegatorList.forEach(delegator -> {
        PoolDetailDelegatorProjection dgTimeAndFee = delegationRepository.getTimeAndFeeByDelegator(
            delegator.getId());
        if (Objects.nonNull(dgTimeAndFee)) {
          delegator.setTime(dgTimeAndFee.getTime());
          delegator.setFee(dgTimeAndFee.getFee());
        }
        delegator.setTotalStake(
            epochStakeRepository.totalStakeByAddressAndPool(delegator.getStakeAddressId(), poolId));
      });
      delegatorResponse.setTotalItems(delegatorPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
    }
    return delegatorResponse;
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }
}
