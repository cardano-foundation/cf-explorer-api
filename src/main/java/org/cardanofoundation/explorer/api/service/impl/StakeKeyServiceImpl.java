package org.cardanofoundation.explorer.api.service.impl;

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
import org.cardanofoundation.explorer.api.model.response.stake.*;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.service.cache.TopDelegatorCacheService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.utils.StringUtils;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
  private final EpochStakeRepository epochStakeRepository;
  private final TopDelegatorCacheService topDelegatorCacheService;

  private final PoolHashRepository poolHashRepository;

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
  @Transactional(readOnly = true)
  public StakeAddressResponse getStakeByAddress(String address) {
    try {
      String stakeAddress = AddressUtils.checkStakeAddress(address);
      return getStake(stakeAddress);
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND);
    }

  }

  @Override
  @Transactional(readOnly = true)
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
    BigInteger stakeTotalBalance
        = addressRepository.findTotalBalanceByStakeAddress(stakeAddress).orElse(BigInteger.ZERO);
    BigInteger stakeRewardWithdrawn = withdrawalRepository.getRewardWithdrawnByStakeAddress(
        stake).orElse(BigInteger.ZERO);
    BigInteger stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
        stake).orElse(BigInteger.ZERO);
    stakeAddressResponse.setRewardWithdrawn(stakeRewardWithdrawn);
    stakeAddressResponse.setRewardAvailable(stakeAvailableReward.subtract(stakeRewardWithdrawn));
    stakeAddressResponse.setTotalStake(stakeTotalBalance.add(stakeAvailableReward)
            .subtract(stakeRewardWithdrawn));
    StakeDelegationProjection poolData = delegationRepository.findPoolDataByAddress(stakeAddress)
        .orElse(null);
    if (poolData != null) {
      DelegationPoolResponse poolResponse = DelegationPoolResponse.builder()
          .poolId(poolData.getPoolId())
          .poolName(poolData.getPoolData())
          .tickerName(poolData.getTickerName())
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
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeDelegationProjection> getDelegationHistories(String stakeKey, Pageable pageable) {
    Page<StakeDelegationProjection> delegations
        = delegationRepository.findDelegationByAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(delegations);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeHistoryProjection> getStakeHistories(String stakeKey,
                                                                      Pageable pageable) {
    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey).orElseThrow(
        () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
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
    Page<StakeHistoryProjection> page = new PageImpl<>(stakeHistoryList.subList(start, end),
        pageable, stakeHistoryList.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(String stakeKey, Pageable pageable) {
    Page<StakeWithdrawalProjection> withdrawalHistories
        = withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(withdrawalHistories);
  }

  @Override
  @Transactional(readOnly = true)
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
    Page<StakeInstantaneousRewardsProjection> page = new PageImpl<>(
        instantaneousRewards.subList(start, end), pageable, instantaneousRewards.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(Pageable pageable) {
    List<Long> topDelegatorStakeIdsCached = topDelegatorCacheService.getTopStakeDelegatorCache();

    List<StakeAddressProjection> stakeList;
    if (topDelegatorStakeIdsCached.isEmpty()) {
      stakeList = stakeAddressRepository.findStakeAddressOrderByBalance(pageable);
    } else {
      stakeList = stakeAddressRepository.findStakeAddressOrderByBalance(topDelegatorStakeIdsCached, pageable);
    }

    Set<String> stakeAddressList = StreamUtil.mapApplySet(stakeList, StakeAddressProjection::getStakeAddress);
    var poolData = delegationRepository.findPoolDataByAddressIn(stakeAddressList);
    var mapPoolByStakeAddress = StreamUtil.toMap(poolData, StakeDelegationProjection::getStakeAddress);

    List<StakeFilterResponse> content = new ArrayList<>();
    for (var stake : stakeList) {
      StakeDelegationProjection delegation = mapPoolByStakeAddress.get(stake.getStakeAddress());
      StakeFilterResponse stakeResponse = stakeAddressMapper.fromStakeAddressAndDelegationProjection(stake, delegation);
      stakeResponse.setPoolName(delegation.getPoolData());
      content.add(stakeResponse);
    }

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
    BigInteger activeStake = null;
    BigInteger liveStake = null;
    if (Boolean.TRUE.equals(isKoiOs)) {
      Set<String> poolIds = fetchRewardDataService.checkAllPoolInfoForPool();
      if (!poolIds.isEmpty()) {
        Boolean isFetch = fetchRewardDataService.fetchPoolInfoForPool(poolIds);
        if (Boolean.TRUE.equals(isFetch)) {
          activeStake = poolInfoRepository.getTotalActiveStake(currentEpoch);
          liveStake = poolInfoRepository.getTotalLiveStake(currentEpoch);
        }
      } else {
        activeStake = poolInfoRepository.getTotalActiveStake(currentEpoch);
        liveStake = poolInfoRepository.getTotalLiveStake(currentEpoch);
      }
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
  public List<StakeAnalyticBalanceResponse> getStakeBalanceAnalytics(String stakeKey,
      AnalyticType type)
      throws ExecutionException, InterruptedException {

    StakeAddress stakeAddress = stakeAddressRepository.findByView(stakeKey)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    List<CompletableFuture<StakeAnalyticBalanceResponse>> futureStakeAnalytics = new ArrayList<>();
    List<LocalDate> dates = getListDateAnalytic(type);
    ExecutorService fixedExecutor = Executors.newFixedThreadPool(STAKE_ANALYTIC_NUMBER);
    final Optional<LocalDate> maxDateAgg = aggregateAddressTxBalanceRepository.getMaxDay();
    dates.forEach(dateItem -> futureStakeAnalytics.add(
        CompletableFuture.supplyAsync(() -> getBalanceOfStake(stakeAddress, dateItem, maxDateAgg),
            fixedExecutor))
    );

    CompletableFuture.allOf(futureStakeAnalytics.toArray(new CompletableFuture[0])).join();
    List<StakeAnalyticBalanceResponse> responses = new ArrayList<>();

    for (CompletableFuture<StakeAnalyticBalanceResponse> fRes : futureStakeAnalytics) {
      responses.add(fRes.get());
    }
    return responses;
  }

  private StakeAnalyticBalanceResponse getBalanceOfStake(
      StakeAddress stakeAddress,LocalDate to, Optional<LocalDate> maxDateAgg) {
    if (maxDateAgg.isEmpty()) {
      BigInteger balance = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(
          stakeAddress,
          Timestamp.valueOf(to.atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);
      return new StakeAnalyticBalanceResponse(to, balance);
    }

    BigInteger balance;
    if (LocalDate.now().isEqual(to)) {
      balance = getBalanceInRangeHaveToday(stakeAddress, to, maxDateAgg.get());
    } else {
      balance = getBalanceInRangePreviousToday(stakeAddress, to, maxDateAgg.get());
    }

    return new StakeAnalyticBalanceResponse(to, balance);
  }

  private BigInteger getBalanceInRangeHaveToday(
      StakeAddress stakeAddress, LocalDate to, LocalDate maxDateAgg) {

    BigInteger todayBalance = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(
            stakeAddress,
            Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX)),
            Timestamp.valueOf(to.atTime(LocalTime.MAX))
        )
        .orElse(BigInteger.ZERO);

    boolean isNotMissingAggregationData = !to.minusDays(1).isAfter(maxDateAgg);
    if (isNotMissingAggregationData) {
      BigInteger rangeToYesterdayBalance = aggregateAddressTxBalanceRepository
          .sumBalanceByStakeAddressId(stakeAddress.getId(), to.minusDays(1))
          .orElse(BigInteger.ZERO);
      return todayBalance.add(rangeToYesterdayBalance);

    } else {
      BigInteger balanceAgg = aggregateAddressTxBalanceRepository
          .sumBalanceByStakeAddressId(stakeAddress.getId(), maxDateAgg)
          .orElse(BigInteger.ZERO);

      BigInteger balanceNotAgg = addressTxBalanceRepository.getBalanceByStakeAddressAndTime(
          stakeAddress,
          Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
          Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);
      return todayBalance.add(balanceAgg).add(balanceNotAgg);
    }
  }

  private BigInteger getBalanceInRangePreviousToday(
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

  private List<LocalDate> getListDateAnalytic(AnalyticType type) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    List<LocalDate> dates = new ArrayList<>();
    switch (type) {
      case ONE_WEEK -> {
        var currentWeek = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = STAKE_ANALYTIC_NUMBER - 1; i >= 0; i--) {
          dates.add(currentWeek.minusWeeks(i));
        }
      }
      case ONE_MONTH -> {
        var currentMonth = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = STAKE_ANALYTIC_NUMBER - 1; i >= 0; i--) {
          dates.add(currentMonth.minusMonths(i));
        }
      }
      case THREE_MONTH -> {
        for (int i = STAKE_ANALYTIC_NUMBER - 1; i >= 0; i--) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
      }
      default -> {
        for (int i = STAKE_ANALYTIC_NUMBER - 1; i >= 0; i--) {
          dates.add(currentDate.minusDays(i));
        }
      }
    }
    return dates;
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
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    MinMaxProjection balanceList = addressTxBalanceRepository.findMinMaxBalanceByStakeAddress(
        stake.getId());
    if (balanceList == null) {
      return Collections.emptyList();
    }
    return List.of(balanceList.getMinVal(), balanceList.getMaxVal());
  }
}
