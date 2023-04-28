package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.StakeRewardType;
import com.cardano.explorer.common.enumeration.StakeTxType;
import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWalletActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeTxProjection;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.StakeKeyLifeCycleService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
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
  private final TxRepository txRepository;

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
                .fee(item.getFee())
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
                .fee(item.getFee())
                .time(item.getTime().toLocalDateTime())
                .value(item.getAmount())
                .build()
        )
    );
  }

  @Override
  public StakeWithdrawalDetailResponse getStakeWithdrawalDetail(String stakeKey, String hash) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var withdrawal = withdrawalRepository.getWithdrawalByAddressAndTx(stakeAddress, hash)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_WITHDRAWAL_NOT_FOUND));
    var totalBalance = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(stakeAddress,
        withdrawal.getTime()).orElse(BigInteger.ZERO);
    var totalReward = rewardRepository.getAvailableRewardByStakeAddressAndEpoch(stakeAddress,
        withdrawal.getEpochNo()).orElse(BigInteger.ZERO);
    var totalWithdrawal = withdrawalRepository.sumByAddrAndTx(stakeAddress, withdrawal.getTxId())
        .orElse(BigInteger.ZERO);
    var rewardAvailable = totalReward.subtract(totalWithdrawal);
    return StakeWithdrawalDetailResponse.builder()
        .fee(withdrawal.getFee())
        .amount(withdrawal.getAmount())
        .time(withdrawal.getTime().toLocalDateTime())
        .txHash(withdrawal.getTxHash())
        .stakeTotalAmount(totalBalance)
        .stakeRewardAvailable(rewardAvailable)
        .build();
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
  public BaseFilterResponse<StakeWalletActivityResponse> getStakeWalletActivities(String stakeKey,
      Pageable pageable) {
    List<StakeWalletActivityResponse> response = new ArrayList<>();
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var txAmountList = addressTxBalanceRepository.findTxAndAmountByStake(stakeAddress.getView(),
        pageable);
    List<Long> txIds = txAmountList.getContent().stream().map(StakeTxProjection::getTxId)
        .collect(Collectors.toList());
    var txList = txRepository.findByIdIn(txIds);
    var registrationList = stakeRegistrationRepository.getStakeRegistrationsByAddressAndTxIn(
        stakeAddress, txIds);
    var deregistrationList = stakeDeRegistrationRepository.getStakeDeRegistrationsByAddressAndTxIn(
        stakeAddress, txIds);
    var delegationList = delegationRepository.findDelegationByAddressAndTxIn(stakeAddress, txIds);
    Map<Long, Tx> txMap = txList.stream().collect(Collectors.toMap(Tx::getId, Function.identity()));
    txAmountList.getContent().forEach(
        item -> {
          StakeWalletActivityResponse stakeWalletActivity = new StakeWalletActivityResponse();
          stakeWalletActivity.setTxHash(txMap.get(item.getTxId()).getHash());
          stakeWalletActivity.setAmount(item.getAmount());
          stakeWalletActivity.setTime(item.getTime().toLocalDateTime());
          if (Boolean.TRUE.equals(txMap.get(item.getTxId()).getValidContract())) {
            stakeWalletActivity.setStatus(TxStatus.SUCCESS);
          } else {
            stakeWalletActivity.setStatus(TxStatus.FAIL);
          }

          stakeWalletActivity.setType(getStakeTxType(stakeWalletActivity, txMap.get(item.getTxId()),
              registrationList, deregistrationList, delegationList));
          response.add(stakeWalletActivity);
        }
    );
    return new BaseFilterResponse<>(txAmountList, response);
  }

  @Override
  public BaseFilterResponse<StakeRewardActivityResponse> getStakeRewardActivities(String stakeKey,
      Pageable pageable) {
    var stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var withdrawList = withdrawalRepository.findEpochWithdrawalByStake(stakeAddress);
    List<StakeRewardActivityResponse> response = withdrawList.stream().map(
        item -> StakeRewardActivityResponse.builder()
            .epochNo(item.getEpoch())
            .amount(item.getAmount())
            .time(item.getTime())
            .type(StakeRewardType.REWARD_WITHDRAWN)
            .build()
    ).collect(Collectors.toList());
    var rewardList = rewardRepository.findRewardByStake(stakeAddress);
    response.addAll(rewardList.stream().map(
        item -> StakeRewardActivityResponse.builder()
            .epochNo(item.getEpoch())
            .amount(item.getAmount())
            .time(item.getTime())
            .type(StakeRewardType.REWARD_RECEIVED)
            .build()
    ).collect(Collectors.toList()));
    if(pageable.getSort().equals(Sort.by(Sort.Direction.ASC, "time"))) {
      response.sort(Comparator.comparing(StakeRewardActivityResponse::getEpochNo));
    } else {
      response.sort(Comparator.comparing(StakeRewardActivityResponse::getEpochNo).reversed());
    }
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), response.size());
    Page<StakeRewardActivityResponse> page = new PageImpl<>(response.subList(start, end),
        pageable, response.size());
    return new BaseFilterResponse<>(page);
  }

  private StakeTxType getStakeTxType(StakeWalletActivityResponse stakeWalletActivity, Tx tx,
      List<Long> registrationList, List<Long> deregistrationList, List<Long> delegationList) {
    boolean isRegistration = registrationList.contains(tx.getId());
    boolean isDeRegistration = deregistrationList.contains(tx.getId());
    boolean isDelegation = delegationList.contains(tx.getId());
    BigInteger fee = tx.getFee();
    BigInteger amount = stakeWalletActivity.getAmount();
    Long deposit = tx.getDeposit();
    if(deposit != null && deposit != 0) {
      return StakeTxType.CERTIFICATE_DEPOSIT_PAID;
    } else if(fee != null && fee.abs().compareTo(stakeWalletActivity.getAmount().abs()) == 0) {
      if (isRegistration || isDeRegistration || isDelegation) {
        return StakeTxType.CERTIFICATE_FEE_PAID;
      } else {
        return StakeTxType.FEE_PAID;
      }
    } else if(amount != null && amount.compareTo(BigInteger.ZERO) < 0) {
      return StakeTxType.SENT;
    } else {
      return StakeTxType.RECEIVED;
    }
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      return null;
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }

}
