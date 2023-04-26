package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.StakeKeyLifeCycleService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyLifeCycleServiceImpl implements StakeKeyLifeCycleService {

  public static final String MIN_TIME = "1970-01-01 00:00:00";
  private final DelegationRepository delegationRepository;
  private final StakeRegistrationRepository stakeRegistrationRepository;
  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RewardRepository rewardRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final AddressTxBalanceRepository addressTxBalanceRepository;

  @Override
  public BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    Page<StakeHistoryProjection> stakeHistoryList =
        stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeAddress,
            condition.getTxHash(), fromDate, toDate, pageable);
    var response = stakeHistoryList.map(item -> StakeRegistrationLifeCycle.builder()
        .txHash(item.getTxHash())
        .fee(item.getFee())
        .deposit(item.getDeposit())
        .time(item.getTime().toLocalDateTime())
        .build()
    );
    return new BaseFilterResponse<>(response);
  }

  @Override
  public BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    Page<StakeHistoryProjection> stakeHistoryList =
        stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeAddress,
            condition.getTxHash(), fromDate, toDate, pageable);
    var response = stakeHistoryList.map(item -> StakeRegistrationLifeCycle.builder()
        .txHash(item.getTxHash())
        .fee(item.getFee())
        .deposit(item.getDeposit())
        .time(item.getTime().toLocalDateTime())
        .build()
    );
    return new BaseFilterResponse<>(response);
  }

  @Override
  public BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    var response = delegationRepository.findDelegationByAddress(stakeAddress, condition.getTxHash(),
        fromDate, toDate, pageable);
    return new BaseFilterResponse<>(
        response.map(
            item -> StakeDelegationFilterResponse.builder()
                .txHash(item.getTxHash())
                .time(item.getTime().toLocalDateTime())
                .outSum(item.getOutSum())
                .build()
        )
    );
  }

  @Override
  public StakeDelegationDetailResponse getStakeDelegationDetail(String stakeKey, String hash) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var delegation = delegationRepository.findDelegationByAddressAndTx(stakeAddress, hash)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_DELEGATION_NOT_FOUND));
    var totalBalance = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(stakeAddress,
        delegation.getTime()).orElse(BigInteger.ZERO);
    return StakeDelegationDetailResponse.builder()
        .fee(delegation.getFee())
        .outSum(delegation.getOutSum())
        .poolId(delegation.getPoolId())
        .poolName(getNameValueFromJson(delegation.getPoolData()))
        .time(delegation.getTime().toLocalDateTime())
        .txHash(delegation.getTxHash())
        .blockNo(delegation.getBlockNo())
        .epoch(delegation.getEpochNo())
        .stakeTotalAmount(totalBalance)
        .build();
  }

  @Override
  public BaseFilterResponse<StakeRewardResponse> getStakeRewards(String stakeKey,
      Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var response
        = rewardRepository.findRewardByStake(stakeAddress, pageable);
    return new BaseFilterResponse<>(response);
  }

  @Override
  public BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawals(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    var response = withdrawalRepository.getWithdrawalByAddress(stakeAddress, condition.getTxHash(),
        fromDate, toDate, pageable);
    return new BaseFilterResponse<>(
        response.map(
            item -> StakeWithdrawalFilterResponse.builder()
                .txHash(item.getTxHash())
                .time(item.getTime().toLocalDateTime())
                .value(item.getAmount())
                .build()
        )
    );
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      return null;
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }
}
