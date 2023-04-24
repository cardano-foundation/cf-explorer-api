package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.request.pool.lifecycle.PoolUpdateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolInfoResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.projection.LifeCycleRewardProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDeRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolInfoProjection;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateDetailProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.EpochRewardProjection;
import com.cardano.explorer.model.response.pool.projection.StakeKeyProjection;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolRetireRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.service.PoolLifecycleService;
import java.math.BigInteger;
import java.util.ArrayList;
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
  public RegistrationAllResponse registration(String poolView) {
    RegistrationAllResponse res = new RegistrationAllResponse();
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    if (Objects.nonNull(poolInfo)) {
      res.setPoolId(poolInfo.getPoolId());
      res.setPoolName(poolInfo.getPoolName());
      res.setPoolView(poolInfo.getPoolView());
    }
    List<PoolRegistrationProjection> poolRegistrations = poolHashRepository.getPoolRegistration(
        poolView);
    List<RegistrationResponse> regisList = new ArrayList<>();
    Set<Long> poolUpdateIds = new HashSet<>();
    if (Objects.nonNull(poolRegistrations)) {
      poolRegistrations.forEach(poolRegis -> {
        poolUpdateIds.add(poolRegis.getPoolUpdateId());
        RegistrationResponse regis = new RegistrationResponse(poolRegis);
        regisList.add(regis);
      });
    }
    List<StakeKeyProjection> stakeKeyProjections = poolUpdateRepository.findOwnerAccountByPoolUpdate(
        poolUpdateIds);
    Map<Long, List<StakeKeyProjection>> stakeKeyProjectionMap = stakeKeyProjections.stream()
        .collect(Collectors.groupingBy(StakeKeyProjection::getPoolUpdateId));
    Map<Long, List<String>> stakeKeyStrMap = new HashMap<>();
    stakeKeyProjectionMap.forEach((k, v) -> stakeKeyStrMap.put(k,
        v.stream().map(StakeKeyProjection::getView).collect(Collectors.toList())));
    regisList.forEach(
        regisRes -> regisRes.setStakeKeys(stakeKeyStrMap.get(regisRes.getPoolUpdateId())));
    res.setRegistrations(regisList);
    return res;
  }

  @Override
  public BaseFilterResponse<PoolUpdateResponse> poolUpdate(PoolUpdateRequest poolUpdateRequest,
      Pageable pageable) {
    BaseFilterResponse<PoolUpdateResponse> res = new BaseFilterResponse<>();
    Page<PoolUpdateProjection> projection = poolUpdateRepository.findPoolUpdateByPool(
        poolUpdateRequest.getPoolView(), poolUpdateRequest.getTxHash(),
        poolUpdateRequest.getFromDate(), poolUpdateRequest.getToDate(), pageable);
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

  @Override
  public PoolUpdateDetailResponse poolUpdateDetail(Long id, Long previousId) {
    PoolUpdateDetailResponse res = null;
    PoolUpdateDetailProjection projection = poolUpdateRepository.findPoolUpdateDetailById(id,
        previousId);
    if (Objects.nonNull(projection)) {
      res = new PoolUpdateDetailResponse(projection);
      res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolUpdate(id));
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
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    if (Objects.nonNull(poolInfo)) {
      res.setPoolId(poolInfo.getPoolId());
      res.setPoolName(poolInfo.getPoolName());
      res.setPoolView(poolInfo.getPoolView());
    }
    res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolView(poolView));
    return res;
  }

  @Override
  public DeRegistrationAllResponse deRegistration(String poolView) {
    DeRegistrationAllResponse res = new DeRegistrationAllResponse();
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    if (Objects.nonNull(poolInfo)) {
      res.setPoolId(poolInfo.getPoolId());
      res.setPoolName(poolInfo.getPoolName());
      res.setPoolView(poolInfo.getPoolView());
    }
    res.setStakeKeys(poolUpdateRepository.findOwnerAccountByPoolView(poolView));
    List<PoolDeRegistrationProjection> projections = poolRetireRepository.getPoolDeRegistration(
        poolView);
    if (Objects.nonNull(projections)) {
      List<DeRegistrationResponse> deRegistrations = new ArrayList<>();
      Set<Integer> epochNos = new HashSet<>();
      projections.forEach(projection -> {
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
        deRegistration.setTotalFee(deRegistration.getFee().add(deRegistration.getPoolHold()));
      });
      res.setDeRegistrations(deRegistrations);
    }
    return res;
  }
}
