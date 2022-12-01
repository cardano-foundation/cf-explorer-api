package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailDelegatorsResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartList;
import com.cardano.explorer.model.response.pool.chart.DelegatorChartResponse;
import com.cardano.explorer.model.response.pool.chart.EpochChartList;
import com.cardano.explorer.model.response.pool.chart.EpochChartResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;
import com.cardano.explorer.model.response.pool.custom.DelegatorDataChart;
import com.cardano.explorer.model.response.pool.custom.EpochDataChart;
import com.cardano.explorer.model.response.pool.custom.PoolDetailDelegator;
import com.cardano.explorer.model.response.pool.custom.PoolDetailEpoch;
import com.cardano.explorer.model.response.pool.custom.TrxBlockEpoch;
import com.cardano.explorer.model.response.pool.custom.TrxPool;
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
import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
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

  @Override
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    Epoch epoch = epochRepository.findByCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer epochNo = epoch.getNo();
    Timestamp endTime = epoch.getEndTime();
    long countDownTime = endTime.getTime() - Timestamp.from(Instant.now()).getTime();
    Integer currentSlot = blockRepository.findCurrentSlotByEpochNo(epochNo);
    BigDecimal totalStake = epochStakeRepository.totalStakeAllPool();
    Integer delegators = delegationRepository.numberDelegatorsAllPool();
    return ResponseEntity.ok(
        DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(currentSlot)
            .liveStake(totalStake).delegators(delegators)
            .countDownEndTime(countDownTime > ZERO ? countDownTime : ZERO).build());
  }

  @Override
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(Integer page,
      Integer size, String search) {
    List<Long> poolIds = poolHashRepository.findAllPoolHashId(page, size, search);
    if (poolIds == null) {
      log.warn("Pool list is empty");
      return null;
    }
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    List<PoolResponse> pools = poolIds.stream().map(PoolResponse::new).collect(Collectors.toList());
    pools.forEach(pool -> {
      List<BigDecimal> pledges = poolUpdateRepository.findPledgeByPool(pool.getPoolId());
      if (pledges != null && !pledges.isEmpty()) {
        pool.setPledge(pledges.get(ZERO));
      }
      List<String> poolNames = poolOfflineDataRepository.findAllByPool(pool.getPoolId());
      if (poolNames != null && !poolNames.isEmpty()) {
        pool.setPoolName(getNameValueFromJson(poolNames.get(ZERO)));
      }
      pool.setPoolSize(epochStakeRepository.totalStakeByPool(pool.getPoolId()));
    });
    Long totalEle = poolHashRepository.totalPoolHashId(page, size, search);
    response.setTotalItems(totalEle);
    response.setData(pools);
    return ResponseEntity.ok(response);
  }

  @Override
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(Long poolId) {
    PoolDetailHeaderResponse poolDetailResponse = new PoolDetailHeaderResponse();
    PoolHash poolHash = poolHashRepository.findById(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolView(poolHash.getView());
    PoolOfflineData poolOff = poolOfflineDataRepository.findFirstByPool(poolHash)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolName(getNameValueFromJson(poolOff.getJson()));
    poolDetailResponse.setTickerName(poolOff.getTickerName());
    poolDetailResponse.setPoolSize(epochStakeRepository.totalStakeByPool(poolId));
    Page<Timestamp> createdTime = blockRepository.getTimeCreatedPool(poolId, PageRequest.of(0, 1));
    if (!createdTime.isEmpty()) {
      poolDetailResponse.setCreateDate(createdTime.toList().get(ZERO));
    }
    List<String> rewardAddressList = poolUpdateRepository.findRewardAccountByPool(poolId);
    if (rewardAddressList != null && !rewardAddressList.isEmpty()) {
      poolDetailResponse.setRewardAccount(rewardAddressList.get(ZERO));
    }
    List<String> ownerAddressList = poolUpdateRepository.findOwnerAccountByPool(poolId);
    if (ownerAddressList != null && !ownerAddressList.isEmpty()) {
      poolDetailResponse.setOwnerAccount(ownerAddressList.get(ZERO));
    }
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    List<PoolUpdate> poolUpdates = poolUpdateRepository.findAllByPoolId(poolId);
    if (poolUpdates != null && !poolUpdates.isEmpty()) {
      poolDetailResponse.setCost(poolUpdates.get(ZERO).getFixedCost());
      poolDetailResponse.setMargin(poolUpdates.get(ZERO).getMargin());
      poolDetailResponse.setPledge(poolUpdates.get(ZERO).getPledge());
    }
    poolDetailResponse.setEpochBlock(epochStakeRepository.countBlockByCurrentEpoch());
    poolDetailResponse.setLifetimeBlock(blockRepository.getCountBlockByPool(poolId));
    return ResponseEntity.ok(poolDetailResponse);
  }

  @Override
  public ResponseEntity<BaseFilterResponse<PoolDetailEpochResponse>> getEpochListForPoolDetail(
      Integer page, Integer size, Long poolId) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    Page<PoolDetailEpoch> epochOfPoolPage = poolHashRepository.findEpochByPool(poolId,
        PageRequest.of(page - 1, size));
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
    return ResponseEntity.ok(epochRes);
  }

  @Override
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(Integer page,
      Integer size) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TrxBlockEpoch> trxBlockEpochPage = stakeRegistrationRepository.getDataForPoolRegistration(
        PageRequest.of(page - 1, size));
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    poolTxRes.forEach(poolTx -> {
      List<TrxPool> trxPools = poolHashRepository.getDataForPoolTx(poolTx.getBlock());
      if (trxPools != null && !trxPools.isEmpty()) {
        TrxPool trxPool = trxPools.get(ZERO);
        poolTx.setPledge(trxPool.getPledge());
        poolTx.setCost(trxPool.getCost());
        poolTx.setMargin(trxPool.getMargin());
        List<String> poolNames = poolOfflineDataRepository.findAllByPool(trxPool.getPoolId());
        if (poolNames != null && !poolNames.isEmpty()) {
          poolTx.setPoolName(getNameValueFromJson(poolNames.get(ZERO)));
        }
      }
    });
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return ResponseEntity.ok(response);
  }

  @Override
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      Integer page, Integer size) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TrxBlockEpoch> trxBlockEpochPage = stakeDeRegistrationRepository.getDataForPoolDeRegistration(
        PageRequest.of(page - 1, size));
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    poolTxRes.forEach(poolTx -> {
      List<TrxPool> trxPools = poolHashRepository.getDataForPoolTx(poolTx.getBlock());
      if (trxPools != null && !trxPools.isEmpty()) {
        TrxPool trxPool = trxPools.get(ZERO);
        poolTx.setPledge(trxPool.getPledge());
        poolTx.setCost(trxPool.getCost());
        poolTx.setMargin(trxPool.getMargin());
        List<String> poolNames = poolOfflineDataRepository.findAllByPool(trxPool.getPoolId());
        if (poolNames != null && !poolNames.isEmpty()) {
          poolTx.setPoolName(getNameValueFromJson(poolNames.get(0)));
        }
      }
    });
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return ResponseEntity.ok(response);
  }

  @Override
  public ResponseEntity<PoolDetailAnalyticsResponse> getAnalyticsForPoolDetail(Long poolId) {
    //Todo set current epoch filter
    EpochChartResponse epochChart = new EpochChartResponse();
    List<BigDecimal> maxStake = poolHashRepository.maxValueEpochChart(poolId,
        PageRequest.of(0, 1));
    if (!maxStake.isEmpty()) {
      epochChart.setHighest(maxStake.get(ZERO));
    }
    List<BigDecimal> minStake = poolHashRepository.minValueEpochChart(poolId,
        PageRequest.of(0, 1));
    if (!minStake.isEmpty()) {
      epochChart.setLowest(minStake.get(ZERO));
    }
    List<EpochDataChart> epochDataCharts = poolHashRepository.getFiveLastDateEpochChart(poolId,
        PageRequest.of(0, 5));
    if (!epochDataCharts.isEmpty()) {
      epochChart.setDataByDays(
          epochDataCharts.stream().map(EpochChartList::new).collect(Collectors.toList()));
    }
    DelegatorChartResponse delegatorChart = new DelegatorChartResponse();
    List<Long> maxDelegator = delegationRepository.maxValueDelegatorChart(poolId,
        PageRequest.of(0, 1));
    if (!maxDelegator.isEmpty()) {
      delegatorChart.setHighest(maxDelegator.get(ZERO));
    }
    List<Long> minDelegator = delegationRepository.minValueDelegatorChart(poolId,
        PageRequest.of(0, 1));
    if (!minDelegator.isEmpty()) {
      delegatorChart.setLowest(minDelegator.get(ZERO));
    }
    List<DelegatorDataChart> delegatorDataCharts = delegationRepository.getFiveLastDateDelegatorChart(
        poolId, PageRequest.of(0, 5));
    if (!delegatorDataCharts.isEmpty()) {
      delegatorChart.setDataByDays(
          delegatorDataCharts.stream().map(DelegatorChartList::new).collect(
              Collectors.toList()));
    }
    return ResponseEntity.ok(
        PoolDetailAnalyticsResponse.builder().epochChart(epochChart).delegatorChart(delegatorChart)
            .build());
  }

  @Override
  public ResponseEntity<PoolDetailDelegatorsResponse> getDelegatorsForPoolDetail(Integer page,
      Integer size, Long poolId) {
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    Page<PoolDetailDelegator> delegatorPage = delegationRepository.getAllDelegatorByPool(poolId,
        PageRequest.of(page - 1, size));
    if (!delegatorPage.isEmpty()) {
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).collect(Collectors.toList());
      delegatorList.forEach(delegator -> {
        PoolDetailDelegator dgTimeAndFee = delegationRepository.getTimeAndFeeByDelegator(
            delegator.getId());
        if (dgTimeAndFee != null) {
          delegator.setTime(dgTimeAndFee.getTime());
          delegator.setFee(dgTimeAndFee.getFee());
        }
        delegator.setTotalStake(delegationRepository.getTotalValueByDelegator(delegator.getId()));
      });
      delegatorResponse.setTotalItems(delegatorPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
    }
    Long countEpoch = poolHashRepository.findEpochByPool(poolId, PageRequest.of(0, 1))
        .getTotalElements();
    return ResponseEntity.ok(
        PoolDetailDelegatorsResponse.builder().delegators(delegatorResponse).totalEpoch(countEpoch)
            .build());
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }
}
