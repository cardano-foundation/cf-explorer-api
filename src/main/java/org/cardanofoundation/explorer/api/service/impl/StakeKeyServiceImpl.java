package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeAddressStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.StakeAddressMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.DelegationPoolResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressRewardDistribution;
import org.cardanofoundation.explorer.api.model.response.stake.*;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.*;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyServiceImpl implements StakeKeyService {

  private final AddressRepository addressRepository;

  private final DelegationRepository delegationRepository;

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RewardRepository rewardRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final TreasuryRepository treasuryRepository;
  private final ReserveRepository reserveRepository;
  private final PoolUpdateRepository poolUpdateRepository;
  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final StakeAddressMapper stakeAddressMapper;
  private final AddressMapper addressMapper;
  private final EpochRepository epochRepository;

  private final RedisTemplate<String, Object> redisTemplate;

  private final PoolInfoRepository poolInfoRepository;

  private final FetchRewardDataService fetchRewardDataService;
  private final AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;

  private static final int STAKE_ANALYTIC_NUMBER = 5;

  @Value("${application.network}")
  private String network;

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeRegistrationRepository.getDataForStakeRegistration(pageable);
    return new BaseFilterResponse<>(trxBlockEpochStakePage.map(StakeTxResponse::new));
  }

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeDeRegistrationRepository.getDataForStakeDeRegistration(pageable);
    return new BaseFilterResponse<>(trxBlockEpochStakePage.map(StakeTxResponse::new));
  }

  @Override
  public StakeAddressResponse getStakeByAddress(String address) {
    try {
      String stakeAddress = AddressUtils.checkStakeAddress(address);
      return getStake(stakeAddress);
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND);
    }

  }

  @Override
  public StakeAddressResponse getStake(String stake) {
    StakeAddressResponse stakeAddressResponse = new StakeAddressResponse();
    StakeAddress stakeAddress
        = stakeAddressRepository.findByView(stake).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stake)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stake);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    stakeAddressResponse.setStakeAddress(stake);
    BigInteger stakeRewardWithdrawn = withdrawalRepository.getRewardWithdrawnByStakeAddress(
        stake).orElse(BigInteger.ZERO);
    BigInteger stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
        stake).orElse(BigInteger.ZERO);
    stakeAddressResponse.setRewardWithdrawn(stakeRewardWithdrawn);
    stakeAddressResponse.setRewardAvailable(stakeAvailableReward.subtract(stakeRewardWithdrawn));
    stakeAddressResponse.setTotalStake(stakeAddress.getBalance().add(stakeAvailableReward)
            .subtract(stakeRewardWithdrawn));
    StakeDelegationProjection poolData = delegationRepository.findPoolDataByAddress(stakeAddress)
        .orElse(null);
    if (poolData != null) {
      DelegationPoolResponse poolResponse = DelegationPoolResponse.builder()
          .poolId(poolData.getPoolId())
          .poolName(poolData.getPoolData())
          .tickerName(poolData.getTickerName())
          .logoUrl(poolData.getLogoUrl())
          .iconUrl(poolData.getIconUrl())
          .build();
      stakeAddressResponse.setPool(poolResponse);
    }
    Long txIdRegister = stakeRegistrationRepository.findMaxTxIdByStake(stakeAddress)
        .orElse(0L);
    Long txIdDeregister = stakeDeRegistrationRepository.findMaxTxIdByStake(stakeAddress)
        .orElse(0L);
    if(txIdRegister.compareTo(txIdDeregister) > 0) {
      stakeAddressResponse.setStatus(StakeAddressStatus.ACTIVE);
    }
    else {
      stakeAddressResponse.setStatus(StakeAddressStatus.DEACTIVATED);
    }
    stakeAddressResponse.setRewardPools(poolUpdateRepository.findPoolByRewardAccount(stakeAddress));
    return stakeAddressResponse;
  }

  @Override
  public BaseFilterResponse<StakeDelegationProjection> getDelegationHistories(String stakeKey, Pageable pageable) {
    Page<StakeDelegationProjection> delegations
        = delegationRepository.findDelegationByAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(delegations);
  }

  @Override
  public BaseFilterResponse<StakeHistoryProjection> getStakeHistories(String stakeKey,
                                                                      Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    List<StakeHistoryProjection> stakeHistoryList =
        stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeAddress);
    stakeHistoryList.addAll(
        stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeAddress));
    stakeHistoryList.sort((o1, o2) -> {
      if (o1.getBlockNo().equals(o2.getBlockNo())) {
        return o2.getBlockIndex() - o1.getBlockIndex();
      } else {
        return o2.getBlockNo().compareTo(o1.getBlockNo());
      }
    });
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), stakeHistoryList.size());
    if (start >= stakeHistoryList.size()) {
      return new BaseFilterResponse<>(new PageImpl<>(List.of()));
    }
    Page<StakeHistoryProjection> page = new PageImpl<>(stakeHistoryList.subList(start, end),
        pageable, stakeHistoryList.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(String stakeKey, Pageable pageable) {
    Page<StakeWithdrawalProjection> withdrawalHistories
        = withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(withdrawalHistories);
  }

  @Override
  public BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(
      String stakeKey, Pageable pageable) {
    List<StakeInstantaneousRewardsProjection> instantaneousRewards
        = treasuryRepository.getTreasuryByAddress(stakeKey);
    instantaneousRewards.addAll(reserveRepository.getReserveByAddress(stakeKey));
    instantaneousRewards.sort((o1, o2) -> {
      if (o1.getBlockNo().equals(o2.getBlockNo())) {
        return o2.getBlockIndex() - o1.getBlockIndex();
      } else {
        return o2.getBlockNo().compareTo(o1.getBlockNo());
      }
    });
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), instantaneousRewards.size());
    if (start >= instantaneousRewards.size()) {
      return new BaseFilterResponse<>(new PageImpl<>(List.of()));
    }
    Page<StakeInstantaneousRewardsProjection> page = new PageImpl<>(
        instantaneousRewards.subList(start, end), pageable, instantaneousRewards.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(Pageable pageable) {
    Pageable pageableDouble = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), pageable.getSort());
    var stakeList = stakeAddressRepository.findStakeAddressOrderByBalance(pageableDouble);
    Set<String> stakeAddressList = StreamUtil.mapApplySet(stakeList, StakeAddressProjection::getStakeAddress);
    if (!fetchRewardDataService.checkRewardAvailable(stakeAddressList.stream().toList())) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeAddressList.stream().toList());
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    var stakeIdList = StreamUtil.mapApplySet(stakeList, StakeAddressProjection::getId);
    var stakeWithdrawals = withdrawalRepository.getRewardWithdrawnByAddrIn(stakeIdList);
    var mapStakeWithdrawnByStakeId = StreamUtil
        .toMap(stakeWithdrawals, StakeWithdrawalProjection::getStakeAddressId, StakeWithdrawalProjection::getAmount);
    var stakeTotalRewards = rewardRepository.getTotalRewardByStakeAddressIn(stakeIdList);
    var mapStakeAvailableRewardsByStakeId = StreamUtil
        .toMap(stakeTotalRewards, StakeRewardProjection::getStakeAddressId, StakeRewardProjection::getAmount);
    var poolData = delegationRepository.findPoolDataByAddressIn(stakeAddressList);
    var mapPoolByStakeAddress = StreamUtil.toMap(poolData, StakeDelegationProjection::getStakeAddress);

    List<StakeFilterResponse> content = new ArrayList<>();
    for (var stake : stakeList) {
      StakeDelegationProjection delegation = mapPoolByStakeAddress.get(stake.getStakeAddress());
      StakeFilterResponse stakeResponse = stakeAddressMapper.fromStakeAddressAndDelegationProjection(stake, delegation);
      stakeResponse.setPoolName(delegation.getPoolData());
      var stakeWithdrawn = mapStakeWithdrawnByStakeId.getOrDefault(stake.getId(), BigInteger.ZERO);
      var stakeTotalReward = mapStakeAvailableRewardsByStakeId.getOrDefault(stake.getId(), BigInteger.ZERO);
      var availableReward = stakeTotalReward.subtract(stakeWithdrawn);
      if(availableReward.compareTo(BigInteger.ZERO) < 0) {
        availableReward = BigInteger.ZERO;
      }
      stakeResponse.setBalance(stake.getTotalStake().add(availableReward));
      content.add(stakeResponse);
    }
    content.sort(Comparator.comparing(StakeFilterResponse::getBalance).reversed());
    Page<StakeFilterResponse> pageResponse = new PageImpl<>(content, pageable, pageable.getPageSize());
    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  public BaseFilterResponse<AddressFilterResponse> getAddresses(String stakeKey,
      Pageable pageable) {
    Page<Address> addresses = addressRepository.findByStakeAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(addresses.map(addressMapper::fromAddressToFilterResponse));
  }

  @Override
  public StakeAnalyticResponse getStakeAnalytics() {
    StakeAnalyticResponse response = new StakeAnalyticResponse();
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElse(0);
    Boolean isKoiOs = fetchRewardDataService.isKoiOs();
    BigInteger activeStake;
    BigInteger liveStake;
    if (Boolean.TRUE.equals(isKoiOs)) {
      activeStake = poolInfoRepository.getTotalActiveStake(currentEpoch);
      liveStake = poolInfoRepository.getTotalLiveStake(currentEpoch);
    } else {
      Object activeStakeObj = redisTemplate.opsForValue()
          .get(CommonConstant.REDIS_TOTAL_ACTIVATE_STAKE + network + "_" + currentEpoch);
      activeStake = Objects.nonNull(activeStakeObj) ? new BigInteger(String.valueOf(activeStakeObj))
          : BigInteger.ZERO;
      Object liveStakeObj = redisTemplate.opsForValue()
          .get(CommonConstant.REDIS_TOTAL_LIVE_STAKE + network);
      liveStake = Objects.nonNull(liveStakeObj) ? new BigInteger(String.valueOf(liveStakeObj))
          : BigInteger.ZERO;
    }
    response.setActiveStake(activeStake);
    response.setLiveStake(liveStake);
    return response;
  }

  @Override
  public List<StakeAnalyticBalanceResponse> getStakeBalanceAnalytics(
      String stakeKey, AnalyticType type) throws ExecutionException, InterruptedException {

    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey)
        .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    List<CompletableFuture<StakeAnalyticBalanceResponse>> futureStakeAnalytics = new ArrayList<>();
    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);
    ExecutorService fixedExecutor = Executors.newFixedThreadPool(5);
    final Optional<LocalDate> maxDateAgg = aggregateAddressTxBalanceRepository.getMaxDay();
    for (int i = 1; i < dates.size(); i++) {
      LocalDateTime analyticTime = dates.get(i);
      futureStakeAnalytics.add(CompletableFuture.supplyAsync(
          () -> getBalanceOfStake(stakeAddress, analyticTime, type, maxDateAgg), fixedExecutor)
      );
    }
    CompletableFuture.allOf(futureStakeAnalytics.toArray(new CompletableFuture[0])).join();
    List<StakeAnalyticBalanceResponse> responses = new ArrayList<>();

    for (CompletableFuture<StakeAnalyticBalanceResponse> fRes : futureStakeAnalytics) {
      responses.add(fRes.get());
    }
    return responses;
  }

  private StakeAnalyticBalanceResponse getBalanceOfStake(
      StakeAddress stakeAddress, LocalDateTime to,
      AnalyticType type, Optional<LocalDate> maxDateAgg) {
    BigInteger balance;
    if (maxDateAgg.isEmpty()) {
      if (type == AnalyticType.ONE_DAY) {
        balance = addressTxBalanceRepository
            .getBalanceByStakeAddressAndTime(stakeAddress, Timestamp.valueOf(to))
            .orElse(BigInteger.ZERO);
      } else {
        balance = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(
            stakeAddress, Timestamp.valueOf(to.toLocalDate().atTime(LocalTime.MAX))
        ).orElse(BigInteger.ZERO);
      }
    } else {
      if (type == AnalyticType.ONE_DAY) {
        LocalDate previousDay = to.toLocalDate().minusDays(1);
        BigInteger previousBalance = getBalanceOfStake(stakeAddress, previousDay, maxDateAgg.get());
        BigInteger extraTimeBalance = addressTxBalanceRepository
            .getBalanceByStakeAddressAndTime(stakeAddress,
                Timestamp.valueOf(previousDay.atTime(LocalTime.MAX)), Timestamp.valueOf(to))
            .orElse(BigInteger.ZERO);
        balance = previousBalance.add(extraTimeBalance);
      } else {
        balance = getBalanceOfStake(stakeAddress, to.toLocalDate(), maxDateAgg.get());
      }
    }
    if (BigInteger.ZERO.equals(balance)) {
      balance = checkNoRecord(stakeAddress, type, to) ? null : balance;
    }
    return new StakeAnalyticBalanceResponse(to, balance);
  }

  private boolean checkNoRecord(StakeAddress stakeAddress, AnalyticType type, LocalDateTime toDateTime) {
    Timestamp endRange;
    if (type == AnalyticType.ONE_DAY) {
      endRange = Timestamp.valueOf(toDateTime);
    } else {
      endRange = Timestamp.valueOf(toDateTime.toLocalDate().atTime(LocalTime.MAX));
    }
    Long numberBalanceRecord = addressTxBalanceRepository.countRecord(stakeAddress, endRange);
    return numberBalanceRecord == null || numberBalanceRecord ==  0;
  }

  private BigInteger getBalanceOfStake(
      StakeAddress stakeAddress, LocalDate to, LocalDate maxDateAgg) {
    boolean isNotMissingAggregationData = !to.isAfter(maxDateAgg);
    if (isNotMissingAggregationData) {
      return aggregateAddressTxBalanceRepository
          .sumBalanceByStakeAddressId(stakeAddress.getId(), to)
          .orElse(BigInteger.ZERO);
    }

    BigInteger balanceAgg = aggregateAddressTxBalanceRepository
        .sumBalanceByStakeAddressId(stakeAddress.getId(), maxDateAgg)
        .orElse(BigInteger.ZERO);

    BigInteger balanceNotAgg = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(
        stakeAddress,
        Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
        Timestamp.valueOf(to.atTime(LocalTime.MAX))
    ).orElse(BigInteger.ZERO);
    return balanceAgg.add(balanceNotAgg);
  }

  @Override
  public List<StakeAnalyticRewardResponse> getStakeRewardAnalytics(String stakeKey) {
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    List<StakeAnalyticRewardResponse> responses = rewardRepository.findRewardByStake(stakeKey);
    Map<Integer, BigInteger> rewardMap = responses.stream().collect(Collectors.toMap(
            StakeAnalyticRewardResponse::getEpoch, StakeAnalyticRewardResponse::getValue));
    responses = new ArrayList<>();
    int startEpoch = rewardMap.keySet().stream().mapToInt(v -> v)
            .min().orElse(0);
    int currentEpoch = epochRepository.findCurrentEpochNo().orElse(2) - 2;
    for (int epoch = startEpoch ; epoch <= currentEpoch; epoch++) {
      StakeAnalyticRewardResponse response;
      response = new StakeAnalyticRewardResponse(epoch,
              rewardMap.getOrDefault(epoch, BigInteger.ZERO));
      responses.add(response);
    }
    return responses;
  }

  @Override
  public List<BigInteger> getStakeMinMaxBalance(String stakeKey) {
    StakeAddress stake = stakeAddressRepository.findByView(stakeKey)
        .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    MinMaxProjection balanceList = addressTxBalanceRepository.findMinMaxBalanceByStakeAddress(
        stake.getId());
    if (balanceList == null) {
      return Collections.emptyList();
    }
    return List.of(balanceList.getMinVal(), balanceList.getMaxVal());
  }

  @Override
  public StakeAddressRewardDistribution getStakeAddressRewardDistributionInfo(String stakeKey) {
    StakeAddressRewardDistribution stakeAddressRewardDistribution = new StakeAddressRewardDistribution();
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    stakeAddressRewardDistribution.setStakeAddress(stakeKey);
    BigInteger stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
        stakeKey).orElse(BigInteger.ZERO);
    stakeAddressRewardDistribution.setRewardAvailable(stakeAvailableReward);
    Set<RewardType> rewardTypeOfStakeKey = rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey);
    if (rewardTypeOfStakeKey.contains(RewardType.MEMBER)){
      stakeAddressRewardDistribution.setHasMemberReward(true);
    }
    if(rewardTypeOfStakeKey.contains(RewardType.LEADER)){
      stakeAddressRewardDistribution.setHasLeaderReward(true);
    }
    return stakeAddressRewardDistribution;
  }
}
