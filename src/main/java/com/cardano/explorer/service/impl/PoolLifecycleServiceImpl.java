package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolInfoResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.lifecycle.TabularRegisResponse;
import com.cardano.explorer.model.response.pool.projection.EpochRewardProjection;
import com.cardano.explorer.model.response.pool.projection.LifeCycleRewardProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDeRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolInfoProjection;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateDetailProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.StakeKeyProjection;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolRetireRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.service.PoolLifecycleService;
import com.sotatek.cardano.common.entity.PoolUpdate;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolLifecycleServiceImpl implements PoolLifecycleService {

  private final StakeAddressRepository stakeAddressRepository;

  private final PoolHashRepository poolHashRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  private final RewardRepository rewardRepository;

  private final PoolRetireRepository poolRetireRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final EpochRepository epochRepository;

  @Override
  public BaseFilterResponse<String> getPoolViewByStakeKey(String stakeKey, Pageable pageable) {
    BaseFilterResponse<String> res = new BaseFilterResponse<>();
    Page<String> poolViews = stakeAddressRepository.getPoolViewByStakeKey(stakeKey,
        pageable);
    res.setData(poolViews.getContent());
    res.setTotalItems(poolViews.getTotalElements());
    return res;
  }

  @Override
  public BaseFilterResponse<PoolUpdateResponse> registration(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable) {
    return getDataForPoolUpdate(poolView, txHash, fromDate, toDate, pageable);
  }

  @Override
  public RegistrationResponse registrationDetail(String poolView, Long id) {
    RegistrationResponse res = new RegistrationResponse();
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    PoolRegistrationProjection projection = poolHashRepository.getPoolRegistration(id);
    if (Objects.nonNull(projection)) {
      res = new RegistrationResponse(projection);
    }
    if (Objects.nonNull(poolInfo)) {
      res.setPoolId(poolInfo.getPoolId());
      res.setPoolName(poolInfo.getPoolName());
      res.setPoolView(poolInfo.getPoolView());
    }
    res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolUpdate(id));
    return res;
  }

  @Override
  public BaseFilterResponse<PoolUpdateResponse> poolUpdate(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable) {
    return getDataForPoolUpdate(poolView, txHash, fromDate, toDate, pageable);
  }

  @Override
  public PoolUpdateDetailResponse poolUpdateDetail(Long id) {
    PoolUpdateDetailResponse res = null;
    PoolUpdateDetailProjection projection = poolUpdateRepository.findPoolUpdateDetailById(id);
    if (Objects.nonNull(projection)) {
      res = new PoolUpdateDetailResponse(projection);
      res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolUpdate(id));
      PoolUpdate poolUpdatePrevious = poolUpdateRepository.findTopByIdLessThanAndPoolHashIdOrderByIdDesc(
          id, projection.getHashId());
      if (Objects.nonNull(poolUpdatePrevious)) {
        res.setPreviousPledge(poolUpdatePrevious.getPledge());
        res.setPreviousMargin(poolUpdatePrevious.getMargin());
      }
    }
    return res;
  }

  @Override
  public BaseFilterResponse<RewardResponse> listReward(String poolView, Pageable pageable) {
    BaseFilterResponse<RewardResponse> res = new BaseFilterResponse<>();
    List<RewardResponse> rewardRes = new ArrayList<>();
    Page<LifeCycleRewardProjection> projections = rewardRepository.getRewardInfoByPool(poolView,
        pageable);
    if (Objects.nonNull(projections)) {
      projections.stream().forEach(projection -> {
        RewardResponse reward = new RewardResponse(projection);
        rewardRes.add(reward);
      });
      res.setTotalItems(projections.getTotalElements());
    }
    res.setData(rewardRes);
    return res;
  }

  @Override
  public PoolInfoResponse poolInfo(String poolView) {
    PoolInfoResponse res = new PoolInfoResponse();
    PoolInfoProjection projection = poolHashRepository.getPoolInfo(poolView);
    if (Objects.nonNull(projection)) {
      res.setPoolId(projection.getPoolId());
      res.setPoolName(projection.getPoolName());
      res.setPoolView(projection.getPoolView());
      res.setPoolSize(epochStakeRepository.activeStakeByPool(projection.getId()));
      res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolView(poolView));
      res.setRewardAvailable(rewardRepository.getTotalRewardByPool(poolView));
    }
    List<Integer> retireEpochs = poolRetireRepository.findByPoolView(poolView);
    if (Objects.isNull(retireEpochs) || retireEpochs.isEmpty()) {
      res.setStatus(CommonConstant.POOL_STATUS_ACTIVE);
      res.setEpochNo(epochRepository.findCurrentEpochNo().orElse(0));
    } else {
      res.setStatus(CommonConstant.POOL_STATUS_RETIRING);
      res.setEpochNo(retireEpochs.get(0));
    }
    return res;
  }

  @Override
  public BaseFilterResponse<DeRegistrationResponse> deRegistration(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable) {
    BaseFilterResponse<DeRegistrationResponse> res = new BaseFilterResponse<>();
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    Timestamp fromTimestamp = null;
    Timestamp toTimestamp = null;
    if (Objects.nonNull(fromDate)) {
      fromTimestamp = new Timestamp(fromDate.getTime());
    }
    if (Objects.nonNull(toDate)) {
      toTimestamp = new Timestamp(toDate.getTime());
    }
    Page<PoolDeRegistrationProjection> projections = poolRetireRepository.getPoolDeRegistration(
        poolView,
        txHash, fromTimestamp, toTimestamp, pageable);
    List<DeRegistrationResponse> deRegistrations = new ArrayList<>();
    if (Objects.nonNull(projections)) {
      Set<Integer> epochNos = new HashSet<>();
      projections.stream().forEach(projection -> {
        DeRegistrationResponse deRegistrationRes = new DeRegistrationResponse(projection);
        deRegistrations.add(deRegistrationRes);
        epochNos.add(projection.getRetiringEpoch());
      });
      List<EpochRewardProjection> epochRewardProjections = rewardRepository.getRewardRefundByEpoch(
          poolView, epochNos);
      Map<Integer, BigInteger> refundAmountMap = new HashMap<>();
      epochRewardProjections.forEach(
          refund -> refundAmountMap.put(refund.getEpochNo(), refund.getAmount()));
      deRegistrations.forEach(deRegistration -> {
        deRegistration.setPoolHold(refundAmountMap.get(deRegistration.getRetiringEpoch()));
        BigInteger totalFee = BigInteger.ZERO;
        if (Objects.nonNull(deRegistration.getPoolHold())) {
          totalFee = totalFee.add(deRegistration.getPoolHold());
        }
        if (Objects.nonNull(deRegistration.getFee())) {
          totalFee = totalFee.add(deRegistration.getFee());
        }
        deRegistration.setTotalFee(totalFee);
        deRegistration.setPoolId(poolInfo.getPoolId());
        deRegistration.setPoolName(poolInfo.getPoolName());
        deRegistration.setPoolView(poolInfo.getPoolView());
        deRegistration.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolView(poolView));
      });
      res.setTotalItems(projections.getTotalElements());
    }
    res.setData(deRegistrations);
    return res;
  }

  @Override
  public BaseFilterResponse<TabularRegisResponse> registrationList(String poolView,
      Pageable pageable) {
    BaseFilterResponse<TabularRegisResponse> res = new BaseFilterResponse<>();
    List<TabularRegisResponse> tabularRegisList = new ArrayList<>();
    Page<PoolRegistrationProjection> projection = poolHashRepository.getPoolRegistrationByPool(
        poolView, pageable);
    if (Objects.nonNull(projection)) {
      Set<Long> poolUpdateIds = new HashSet<>();
      projection.stream().forEach(tabularRegis -> {
        tabularRegisList.add(new TabularRegisResponse(tabularRegis));
        poolUpdateIds.add(tabularRegis.getPoolUpdateId());
      });
      List<StakeKeyProjection> stakeKeyProjections = poolUpdateRepository.findOwnerAccountByPoolUpdate(
          poolUpdateIds);
      Map<Long, List<StakeKeyProjection>> stakeKeyProjectionMap = stakeKeyProjections.stream()
          .collect(Collectors.groupingBy(StakeKeyProjection::getPoolUpdateId));
      Map<Long, List<String>> stakeKeyStrMap = new HashMap<>();
      stakeKeyProjectionMap.forEach((k, v) -> stakeKeyStrMap.put(k,
          v.stream().map(StakeKeyProjection::getView).collect(Collectors.toList())));
      res.setTotalItems(projection.getTotalElements());
      tabularRegisList.forEach(tabularRegis -> tabularRegis.setStakeKeys(
          stakeKeyStrMap.get(tabularRegis.getPoolUpdateId())));
    }
    res.setData(tabularRegisList);
    return res;
  }

  @Override
  public BaseFilterResponse<PoolUpdateDetailResponse> poolUpdateList(String poolView,
      Pageable pageable) {
    BaseFilterResponse<PoolUpdateDetailResponse> res = new BaseFilterResponse<>();
    List<PoolUpdateDetailResponse> poolUpdateList = new ArrayList<>();
    Page<PoolUpdateDetailProjection> projection = poolUpdateRepository.findPoolUpdateByPool(
        poolView, pageable);
    if (Objects.nonNull(projection)) {
      projection.stream().forEach(poolUpdate -> {
        PoolUpdateDetailResponse poolUpdateRes = new PoolUpdateDetailResponse(poolUpdate);
        poolUpdateRes.setStakeKeys(
            poolUpdateRepository.findOwnerAccountByPoolUpdate(poolUpdate.getPoolUpdateId()));
        PoolUpdate poolUpdatePrevious = poolUpdateRepository.findTopByIdLessThanAndPoolHashIdOrderByIdDesc(
            poolUpdate.getPoolUpdateId(), poolUpdate.getHashId());
        if (Objects.nonNull(poolUpdatePrevious)) {
          poolUpdateRes.setPreviousPledge(poolUpdatePrevious.getPledge());
          poolUpdateRes.setPreviousMargin(poolUpdatePrevious.getMargin());
        }
        poolUpdateList.add(poolUpdateRes);
        res.setTotalItems(projection.getTotalElements());
      });
    }
    res.setData(poolUpdateList);
    return res;
  }

  private BaseFilterResponse<PoolUpdateResponse> getDataForPoolUpdate(String poolView,
      String txHash,
      Date fromDate, Date toDate,
      Pageable pageable) {
    BaseFilterResponse<PoolUpdateResponse> res = new BaseFilterResponse<>();
    Timestamp fromTimestamp = null;
    Timestamp toTimestamp = null;
    if (Objects.nonNull(fromDate)) {
      fromTimestamp = new Timestamp(fromDate.getTime());
    }
    if (Objects.nonNull(toDate)) {
      toTimestamp = new Timestamp(toDate.getTime());
    }
    Page<PoolUpdateProjection> projection = poolUpdateRepository.findPoolUpdateByPool(poolView,
        txHash, fromTimestamp, toTimestamp, pageable);
    List<PoolUpdateResponse> poolUpdateResList = new ArrayList<>();
    if (Objects.nonNull(projection)) {
      projection.stream().forEach(poolUpdate -> {
        PoolUpdateResponse poolUpdateRes = new PoolUpdateResponse(poolUpdate);
        poolUpdateResList.add(poolUpdateRes);
      });
      res.setTotalItems(projection.getTotalElements());
    }
    res.setData(poolUpdateResList);
    return res;
  }
}
