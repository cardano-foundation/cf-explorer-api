package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
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
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolInfoResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.SPOStatusResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.LifeCycleRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
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

  private final EpochRepository epochRepository;

  private final FetchRewardDataService fetchRewardDataService;

  private final PoolInfoRepository poolInfoRepository;

  private final RedisTemplate<String, Object> redisTemplate;

  @Value("${application.network}")
  private String network;

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
    return getDataForPoolUpdate(poolView, txHash, fromDate, toDate, pageable, 0);
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
    return getDataForPoolUpdate(poolView, txHash, fromDate, toDate, pageable, 1);
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
    if (Boolean.TRUE.equals(fetchRewardDataService.isKoiOs())) {
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolView);
      if (Boolean.FALSE.equals(fetchRewardDataService.checkRewardForPool(rewardAccounts))
          && Boolean.FALSE.equals(fetchRewardDataService.fetchRewardForPool(rewardAccounts))) {
        return res;
      }
    }
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
      res.setRewardAccounts(poolUpdateRepository.findRewardAccountByPoolId(projection.getId()));
      Integer epochNo = epochRepository.findCurrentEpochNo().orElse(null);
      if (Boolean.TRUE.equals(fetchRewardDataService.isKoiOs())) {
        Set<String> poolIdList = new HashSet<>(Collections.singletonList(poolView));
        if (Boolean.FALSE.equals(fetchRewardDataService.checkPoolInfoForPool(poolIdList))) {
          fetchRewardDataService.fetchPoolInfoForPool(poolIdList);
        }
        res.setPoolSize(poolInfoRepository.getActiveStakeByPoolAndEpoch(poolView, epochNo));
        Boolean isReward = fetchRewardDataService.checkRewardForPool(res.getRewardAccounts());
        if (Boolean.FALSE.equals(isReward)) {
          Boolean isFetch = fetchRewardDataService.fetchRewardForPool(res.getRewardAccounts());
          if (Boolean.FALSE.equals(isFetch)) {
            res.setRewardAvailable(BigInteger.ZERO);
          }
        }
      } else {
        res.setPoolSize(getStakeFromCache(poolView, epochNo));
      }
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
    if (Objects.nonNull(txHash) && txHash.isBlank()) {
      txHash = null;
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
      boolean isKoiOs = fetchRewardDataService.isKoiOs();
      if (isKoiOs) {
        List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolId(poolInfo.getId());
        boolean isReward = fetchRewardDataService.checkRewardForPool(rewardAccounts);
        if (!isReward) {
          fetchRewardDataService.fetchRewardForPool(rewardAccounts);
        }
      }
      List<EpochRewardProjection> epochRewardProjections = rewardRepository.getRewardRefundByEpoch(
          poolView, epochNos);
      Map<Integer, BigInteger> refundAmountMap = new HashMap<>();
      epochRewardProjections.forEach(
          refund -> refundAmountMap.put(refund.getEpochNo(), refund.getAmount()));
      List<String> stakeKeys = poolUpdateRepository.findOwnerAccountByPoolView(poolView);
      deRegistrations.forEach(deRegistration -> {
        if (deRegistration.isRefundFlag()) {
          deRegistration.setPoolHold(refundAmountMap.get(deRegistration.getRetiringEpoch()));
        }
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
        deRegistration.setStakeKeys(stakeKeys);
      });
      res.setTotalItems(projections.getTotalElements());
      res.setCurrentPage(projections.getNumber());
      res.setTotalPages(projections.getTotalPages());
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
          v.stream().map(StakeKeyProjection::getView).toList()));
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

  @Override
  public SPOStatusResponse poolLifecycleStatus(String poolView) {
    SPOStatusResponse response = new SPOStatusResponse();
    Integer countPoolUpdate = poolUpdateRepository.countPoolUpdateByPool(poolView);
    if (Objects.isNull(countPoolUpdate) || countPoolUpdate == 0) {
      response.setIsRegistration(false);
      response.setIsUpdate(false);
    } else if (countPoolUpdate == 1) {
      response.setIsRegistration(true);
      response.setIsUpdate(false);
    } else {
      response.setIsRegistration(true);
      response.setIsUpdate(true);
    }
    PoolHash pool = poolHashRepository.findByView(poolView).orElseThrow(() -> new BusinessException(
        CommonErrorCode.UNKNOWN_ERROR));
    if (fetchRewardDataService.isKoiOs()) {
      List<String> rewardAccounts = poolUpdateRepository.findRewardAccountByPoolView(poolView);
      if (!fetchRewardDataService.checkRewardForPool(rewardAccounts)) {
        fetchRewardDataService.fetchRewardForPool(rewardAccounts);
      }
    }
    response.setIsReward(rewardRepository.existsByPoolAndType(pool, RewardType.LEADER));
    response.setIsDeRegistration(poolRetireRepository.existsByPoolHash(pool));
    return response;
  }

  private BaseFilterResponse<PoolUpdateResponse> getDataForPoolUpdate(String poolView,
      String txHash,
      Date fromDate, Date toDate,
      Pageable pageable, Integer type) {
    BaseFilterResponse<PoolUpdateResponse> res = new BaseFilterResponse<>();
    Timestamp fromTimestamp = null;
    Timestamp toTimestamp = null;
    if (Objects.nonNull(fromDate)) {
      fromTimestamp = new Timestamp(fromDate.getTime());
    }
    if (Objects.nonNull(toDate)) {
      toTimestamp = new Timestamp(toDate.getTime());
    }
    if (Objects.nonNull(txHash) && txHash.isBlank()) {
      txHash = null;
    }
    Page<PoolUpdateProjection> projection = null;
    if (type == 0) {
      projection = poolUpdateRepository.findPoolRegistrationByPool(poolView,
          txHash, fromTimestamp, toTimestamp, pageable);
    } else {
      projection = poolUpdateRepository.findPoolUpdateByPool(poolView,
          txHash, fromTimestamp, toTimestamp, pageable);
    }
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

  private BigInteger getStakeFromCache(String poolView,
      Integer epochNo) {
    BigInteger stakeFromCache = BigInteger.ZERO;
    if (Objects.isNull(poolView)) {
      return stakeFromCache;
    }
    List<Object> poolIds = List.of(poolView);
    String key = CommonConstant.ACTIVATE_STAKE + network + "_" + epochNo;
    List<Object> objStakeList = null;
    try {
      objStakeList = redisTemplate.opsForHash().multiGet(key, poolIds);
    } catch (Exception e) {
      log.info("Error when get stake from cache with Key=" + key);
      return stakeFromCache;
    }
    if (!objStakeList.isEmpty()) {
      Object objStake = objStakeList.get(0);
      stakeFromCache = (BigInteger) objStake;

    }
    return stakeFromCache;
  }
}
