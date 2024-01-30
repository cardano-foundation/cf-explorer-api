package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
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

import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.*;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.StakeKeyLifeCycleService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;

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
  private final TxOutRepository txOutRepository;
  private final FetchRewardDataService fetchRewardDataService;
  private final EpochParamRepository epochParamRepository;

  @Override
  public StakeLifecycleResponse getStakeLifeCycle(String stakeKey) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    BigInteger totalOperatorReward =
        rewardRepository
            .getTotalRewardByStakeAddressAndType(stakeAddress, RewardType.LEADER)
            .orElse(BigInteger.ZERO);
    BigInteger totalDelegatorReward =
        rewardRepository
            .getTotalRewardByStakeAddressAndType(stakeAddress, RewardType.MEMBER)
            .orElse(BigInteger.ZERO);
    return StakeLifecycleResponse.builder()
        .hasRegistration(stakeRegistrationRepository.existsByAddr(stakeAddress))
        .hasDeRegistration(stakeDeRegistrationRepository.existsByAddr(stakeAddress))
        .hasDelegation(delegationRepository.existsByAddress(stakeAddress))
        .hashRewards(rewardRepository.existsByAddr(stakeAddress))
        .hasWithdrawal(withdrawalRepository.existsByAddr(stakeAddress))
        .totalOperatorRewards(totalOperatorReward)
        .totalDelegatorRewards(totalDelegatorReward)
        .build();
  }

  @Override
  public BaseFilterResponse<StakeRegistrationFilterResponse> getStakeRegistrations(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    if (Objects.nonNull(condition.getTxHash()) && condition.getTxHash().isBlank()) {
      condition.setTxHash(null);
    }
    Page<StakeHistoryProjection> stakeHistoryList =
        stakeRegistrationRepository.getStakeRegistrationsByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable);
    var epochNoList = stakeHistoryList.stream().map(StakeHistoryProjection::getEpochNo).toList();
    var epochParams = epochParamRepository.findByEpochNoIn(epochNoList);
    Map<Integer, BigInteger> epochNoDepositMap =
        epochParams.stream()
            .collect(Collectors.toMap(EpochParam::getEpochNo, EpochParam::getKeyDeposit));
    var response =
        stakeHistoryList.map(
            item ->
                StakeRegistrationFilterResponse.builder()
                    .txHash(item.getTxHash())
                    .fee(item.getFee())
                    .deposit(epochNoDepositMap.get(item.getEpochNo()).longValue())
                    .time(item.getTime().toLocalDateTime())
                    .build());
    return new BaseFilterResponse<>(response);
  }

  @Override
  public StakeRegistrationDetailResponse getStakeRegistrationDetail(String stakeKey, String hash) {
    StakeHistoryProjection stakeHistoryProjection =
        stakeRegistrationRepository
            .findByAddressAndTx(stakeKey, hash)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_REGISTRATION_NOT_FOUND));
    Long deposit =
        epochParamRepository
            .findKeyDepositByEpochNo(stakeHistoryProjection.getEpochNo())
            .longValue();
    BigInteger totalInput =
        txOutRepository
            .sumValueInputByTxAndStakeAddress(stakeHistoryProjection.getTxHash(), stakeKey)
            .orElse(BigInteger.ZERO);
    boolean joinDepositPaid = totalInput.compareTo(BigInteger.ZERO) > BigInteger.ZERO.intValue();
    return StakeRegistrationDetailResponse.builder()
        .txHash(stakeHistoryProjection.getTxHash())
        .fee(stakeHistoryProjection.getFee())
        .deposit(deposit)
        .time(stakeHistoryProjection.getTime().toLocalDateTime())
        .joinDepositPaid(joinDepositPaid)
        .build();
  }

  @Override
  public BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegations(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    if (Objects.nonNull(condition.getTxHash()) && condition.getTxHash().isBlank()) {
      condition.setTxHash(null);
    }
    var response =
        delegationRepository.findDelegationByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable);
    return new BaseFilterResponse<>(
        response.map(
            item ->
                StakeDelegationFilterResponse.builder()
                    .txHash(item.getTxHash())
                    .fee(item.getFee())
                    .time(item.getTime().toLocalDateTime())
                    .outSum(item.getOutSum())
                    .poolName(item.getPoolName())
                    .poolId(item.getPoolId())
                    .build()));
  }

  @Override
  public StakeDelegationDetailResponse getStakeDelegationDetail(String stakeKey, String hash) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var delegation =
        delegationRepository
            .findDelegationByAddressAndTx(stakeAddress, hash)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_DELEGATION_NOT_FOUND));
    var totalBalance =
        addressTxBalanceRepository
            .getBalanceByStakeAddressAndTime(stakeAddress, delegation.getTime())
            .orElse(BigInteger.ZERO);
    return StakeDelegationDetailResponse.builder()
        .fee(delegation.getFee())
        .outSum(delegation.getOutSum())
        .poolId(delegation.getPoolId())
        .poolName(delegation.getPoolData())
        .time(delegation.getTime().toLocalDateTime())
        .txHash(delegation.getTxHash())
        .blockNo(delegation.getBlockNo())
        .epoch(delegation.getEpochNo())
        .stakeTotalAmount(totalBalance)
        .build();
  }

  @Override
  public BaseFilterResponse<StakeRewardResponse> getStakeRewards(
      String stakeKey, Date fromDate, Date toDate, RewardType type, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    Timestamp fromTime = Timestamp.valueOf(MIN_TIME);
    Timestamp toTime =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(fromDate)) {
      fromTime = Timestamp.from(fromDate.toInstant());
    }
    if (Objects.nonNull(toDate)) {
      toTime = Timestamp.from(toDate.toInstant());
    }
    var response =
        rewardRepository.findRewardByStake(stakeAddress, fromTime, toTime, type, pageable);
    return new BaseFilterResponse<>(response);
  }

  @Override
  public BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawals(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    if (Objects.nonNull(condition.getTxHash()) && condition.getTxHash().isBlank()) {
      condition.setTxHash(null);
    }
    var response =
        withdrawalRepository.getWithdrawalByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable);
    return new BaseFilterResponse<>(
        response.map(
            item ->
                StakeWithdrawalFilterResponse.builder()
                    .txHash(item.getTxHash())
                    .fee(item.getFee())
                    .time(item.getTime().toLocalDateTime())
                    .value(item.getAmount())
                    .build()));
  }

  @Override
  public StakeWithdrawalDetailResponse getStakeWithdrawalDetail(String stakeKey, String hash) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    var withdrawal =
        withdrawalRepository
            .getWithdrawalByAddressAndTx(stakeAddress, hash)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_WITHDRAWAL_NOT_FOUND));
    var totalBalance =
        addressTxBalanceRepository
            .getBalanceByStakeAddressAndTime(stakeAddress, withdrawal.getTime())
            .orElse(BigInteger.ZERO);
    var totalReward =
        rewardRepository
            .getAvailableRewardByStakeAddressAndEpoch(stakeAddress, withdrawal.getEpochNo())
            .orElse(BigInteger.ZERO);
    var totalWithdrawal =
        withdrawalRepository
            .sumByAddrAndTx(stakeAddress, withdrawal.getTxId())
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
  public BaseFilterResponse<StakeRegistrationFilterResponse> getStakeDeRegistrations(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    if (Objects.nonNull(condition.getTxHash()) && condition.getTxHash().isBlank()) {
      condition.setTxHash(null);
    }
    Page<StakeHistoryProjection> stakeHistoryList =
        stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable);
    var epochNoList = stakeHistoryList.stream().map(StakeHistoryProjection::getEpochNo).toList();
    var epochParams = epochParamRepository.findByEpochNoIn(epochNoList);
    Map<Integer, BigInteger> epochNoDepositMap =
        epochParams.stream()
            .collect(Collectors.toMap(EpochParam::getEpochNo, EpochParam::getKeyDeposit));
    var response =
        stakeHistoryList.map(
            item ->
                StakeRegistrationFilterResponse.builder()
                    .txHash(item.getTxHash())
                    .fee(item.getFee())
                    .deposit(
                        epochNoDepositMap
                            .get(item.getEpochNo())
                            .multiply(BigInteger.valueOf(-1L))
                            .longValue())
                    .time(item.getTime().toLocalDateTime())
                    .build());
    return new BaseFilterResponse<>(response);
  }

  @Override
  public StakeRegistrationDetailResponse getStakeDeRegistrationDetail(
      String stakeKey, String hash) {
    StakeHistoryProjection stakeHistoryProjection =
        stakeDeRegistrationRepository
            .findByAddressAndTx(stakeKey, hash)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_DE_REGISTRATION_NOT_FOUND));
    Long deposit =
        epochParamRepository
            .findKeyDepositByEpochNo(stakeHistoryProjection.getEpochNo())
            .multiply(BigInteger.valueOf(-1))
            .longValue();
    BigInteger totalOutput =
        txOutRepository
            .sumValueOutputByTxAndStakeAddress(stakeHistoryProjection.getTxHash(), stakeKey)
            .orElse(BigInteger.ZERO);
    boolean joinDepositPaid = totalOutput.compareTo(BigInteger.ZERO) > BigInteger.ZERO.intValue();
    return StakeRegistrationDetailResponse.builder()
        .txHash(stakeHistoryProjection.getTxHash())
        .fee(stakeHistoryProjection.getFee())
        .deposit(deposit)
        .time(stakeHistoryProjection.getTime().toLocalDateTime())
        .joinDepositPaid(joinDepositPaid)
        .build();
  }

  @Override
  public BaseFilterResponse<StakeWalletActivityResponse> getStakeWalletActivities(
      String stakeKey, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    var txAmountList =
        addressTxBalanceRepository.findTxAndAmountByStake(stakeAddress.getView(), pageable);
    List<StakeWalletActivityResponse> response =
        getStakeWalletActivitiesContent(stakeAddress, txAmountList);
    return new BaseFilterResponse<>(txAmountList, response);
  }

  @Override
  public BaseFilterResponse<StakeRewardActivityResponse> getStakeRewardActivities(
      String stakeKey, Pageable pageable) {
    var stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    var withdrawList = withdrawalRepository.findEpochWithdrawalByStake(stakeAddress);
    List<StakeRewardActivityResponse> response =
        withdrawList.stream()
            .map(
                item ->
                    StakeRewardActivityResponse.builder()
                        .epochNo(item.getEpoch())
                        .amount(item.getAmount())
                        .time(item.getTime())
                        .type(StakeRewardType.REWARD_WITHDRAWN)
                        .build())
            .collect(Collectors.toList());
    var rewardList = rewardRepository.findRewardByStake(stakeAddress);
    response.addAll(
        rewardList.stream()
            .map(
                item ->
                    StakeRewardActivityResponse.builder()
                        .epochNo(item.getEpoch())
                        .amount(item.getAmount())
                        .time(item.getTime())
                        .type(StakeRewardType.REWARD_RECEIVED)
                        .build())
            .collect(Collectors.toList()));
    if (pageable.getSort().equals(Sort.by(Sort.Direction.ASC, "time"))) {
      response.sort(Comparator.comparing(StakeRewardActivityResponse::getEpochNo));
    } else {
      response.sort(Comparator.comparing(StakeRewardActivityResponse::getEpochNo).reversed());
    }
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), response.size());
    Page<StakeRewardActivityResponse> page =
        new PageImpl<>(response.subList(start, end), pageable, response.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  public BaseFilterResponse<StakeWalletActivityResponse> getStakeWalletActivitiesByDateRange(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    if (Objects.nonNull(condition.getFromDate())) {
      fromDate = Timestamp.from(condition.getFromDate().toInstant());
    }
    if (Objects.nonNull(condition.getToDate())) {
      toDate = Timestamp.from(condition.getToDate().toInstant());
    }
    var txAmountList =
        addressTxBalanceRepository.findTxAndAmountByStakeAndDateRange(
            stakeAddress.getView(), fromDate, toDate, pageable);

    List<StakeWalletActivityResponse> response =
        getStakeWalletActivitiesContent(stakeAddress, txAmountList);
    return new BaseFilterResponse<>(txAmountList, response);
  }

  private List<StakeWalletActivityResponse> getStakeWalletActivitiesContent(
      StakeAddress stakeAddress, Page<StakeTxProjection> txAmountList) {
    List<StakeWalletActivityResponse> response = new ArrayList<>();
    List<Long> txIds =
        txAmountList.getContent().stream()
            .map(StakeTxProjection::getTxId)
            .collect(Collectors.toList());
    var txList = txRepository.findByIdIn(txIds);
    var registrationList =
        stakeRegistrationRepository.getStakeRegistrationsByAddressAndTxIn(stakeAddress, txIds);
    var deregistrationList =
        stakeDeRegistrationRepository.getStakeDeRegistrationsByAddressAndTxIn(stakeAddress, txIds);
    var delegationList = delegationRepository.findDelegationByAddressAndTxIn(stakeAddress, txIds);
    var withdrawList = withdrawalRepository.getWithdrawalByAddressAndTxIn(stakeAddress, txIds);
    Map<Long, Tx> txMap = txList.stream().collect(Collectors.toMap(Tx::getId, Function.identity()));
    txAmountList
        .getContent()
        .forEach(
            item -> {
              StakeWalletActivityResponse stakeWalletActivity = new StakeWalletActivityResponse();
              stakeWalletActivity.setTxHash(txMap.get(item.getTxId()).getHash());
              stakeWalletActivity.setAmount(item.getAmount());
              stakeWalletActivity.setTime(item.getTime().toLocalDateTime());
              stakeWalletActivity.setFee(txMap.get(item.getTxId()).getFee());
              if (Boolean.TRUE.equals(txMap.get(item.getTxId()).getValidContract())) {
                stakeWalletActivity.setStatus(TxStatus.SUCCESS);
              } else {
                stakeWalletActivity.setStatus(TxStatus.FAILED);
              }

              stakeWalletActivity.setType(
                  getStakeTxType(
                      stakeWalletActivity,
                      txMap.get(item.getTxId()),
                      registrationList,
                      deregistrationList,
                      delegationList,
                      withdrawList));
              response.add(stakeWalletActivity);
            });
    return response;
  }

  private StakeTxType getStakeTxType(
      StakeWalletActivityResponse stakeWalletActivity,
      Tx tx,
      List<Long> registrationList,
      List<Long> deregistrationList,
      List<Long> delegationList,
      List<Long> withdrawList) {
    boolean isRegistration = registrationList.contains(tx.getId());
    boolean isDeRegistration = deregistrationList.contains(tx.getId());
    boolean isDelegation = delegationList.contains(tx.getId());
    boolean isWithdraw = withdrawList.contains(tx.getId());
    BigInteger fee = tx.getFee();
    BigInteger amount = stakeWalletActivity.getAmount();
    if (isWithdraw) {
      if (isRegistration) {
        return StakeTxType.REWARD_WITHDRAWN_AND_CERTIFICATE_HOLD_PAID;
      } else if (isDeRegistration) {
        return StakeTxType.REWARD_WITHDRAWN_AND_CERTIFICATE_HOLD_DEPOSIT_REFUNDED;
      } else {
        return StakeTxType.REWARD_WITHDRAWN;
      }
    } else {
      if (isRegistration) {
        return StakeTxType.CERTIFICATE_HOLD_PAID;
      } else if (isDeRegistration) {
        return StakeTxType.CERTIFICATE_HOLD_DEPOSIT_REFUNDED;
      } else if (isDelegation) {
        return StakeTxType.CERTIFICATE_FEE_PAID;
      } else if (Objects.nonNull(fee)
          && fee.abs().compareTo(stakeWalletActivity.getAmount().abs()) == 0) {
        return StakeTxType.FEE_PAID;
      } else if (amount != null && amount.compareTo(BigInteger.ZERO) < 0) {
        return StakeTxType.SENT;
      } else if (amount != null && amount.compareTo(BigInteger.ZERO) > 0) {
        return StakeTxType.RECEIVED;
      }
    }
    return StakeTxType.UNKNOWN;
  }
}
