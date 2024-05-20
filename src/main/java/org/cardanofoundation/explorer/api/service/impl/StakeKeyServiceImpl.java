package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeAddressStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.mapper.StakeAddressMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.address.*;
import org.cardanofoundation.explorer.api.model.response.stake.*;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestStakeAddressBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ReserveRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TreasuryRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.entity.enumeration.RewardType;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestStakeAddressBalance;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeDeregistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistration;
import org.cardanofoundation.explorer.common.exception.BusinessException;

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
  private final StakeAddressMapper stakeAddressMapper;
  private final EpochRepository epochRepository;
  private final TxRepository txRepository;
  private final StakeTxBalanceRepository stakeTxBalanceRepository;

  private final PoolInfoRepository poolInfoRepository;

  private final FetchRewardDataService fetchRewardDataService;
  private final AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;
  private final AddressTxAmountRepository addressTxAmountRepository;
  private final LatestStakeAddressBalanceRepository latestStakeAddressBalanceRepository;

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable) {

    Page<StakeRegistration> stakeRegistrationPage = stakeRegistrationRepository.findAll(pageable);
    Page<StakeTxResponse> stakeTxResponsePage = stakeRegistrationPage.map(StakeTxResponse::new);
    getDetailInfoStakeTxResponse(stakeTxResponsePage);
    return new BaseFilterResponse<>(stakeTxResponsePage);
  }

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable) {

    Page<StakeDeregistration> stakeDeregistrationPage =
        stakeDeRegistrationRepository.findAll(pageable);
    Page<StakeTxResponse> stakeTxResponsePage = stakeDeregistrationPage.map(StakeTxResponse::new);
    getDetailInfoStakeTxResponse(stakeTxResponsePage);
    return new BaseFilterResponse<>(stakeTxResponsePage);
  }

  private void getDetailInfoStakeTxResponse(Page<StakeTxResponse> stakeTxResponsePage) {
    Set<Long> txIds =
        stakeTxResponsePage.stream().map(StakeTxResponse::getTxId).collect(Collectors.toSet());
    List<TxIOProjection> txList = txRepository.findTxIn(txIds);
    Map<Long, TxIOProjection> txMap =
        txList.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));

    Set<Long> stakeAddressIds =
        stakeTxResponsePage.stream()
            .map(StakeTxResponse::getStakeAddressId)
            .collect(Collectors.toSet());
    List<StakeAddress> stakeAddressList = stakeAddressRepository.findAllById(stakeAddressIds);
    Map<Long, StakeAddress> stakeAddressMap =
        stakeAddressList.stream()
            .collect(Collectors.toMap(StakeAddress::getId, Function.identity()));

    stakeTxResponsePage.forEach(
        item -> {
          TxIOProjection txIOProjection = txMap.get(item.getTxId());
          StakeAddress stakeAddress = stakeAddressMap.get(item.getStakeAddressId());
          item.setTxHash(txIOProjection.getHash());
          item.setEpoch(txIOProjection.getEpochNo());
          item.setBlock(txIOProjection.getBlockNo());
          item.setEpochSlotNo(txIOProjection.getEpochSlotNo());
          item.setTxTime(Timestamp.valueOf(txIOProjection.getTime()));
          item.setSlotNo(txIOProjection.getSlot());
          item.setStakeKey(stakeAddress.getView());
        });
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
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stake)
            .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    LatestStakeAddressBalance latestStakeAddressBalance =
        latestStakeAddressBalanceRepository
            .findByStakeAddress(stake)
            .orElse(
                LatestStakeAddressBalance.builder()
                    .address(stake)
                    .quantity(BigInteger.ZERO)
                    .build());

    if (!fetchRewardDataService.checkRewardAvailable(stake)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stake);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    stakeAddressResponse.setStakeAddress(stake);
    if (Boolean.TRUE.equals(fetchRewardDataService.useKoios())) {
      BigInteger stakeRewardWithdrawn =
          withdrawalRepository.getRewardWithdrawnByStakeAddress(stake).orElse(BigInteger.ZERO);
      BigInteger stakeAvailableReward =
          rewardRepository.getAvailableRewardByStakeAddress(stake).orElse(BigInteger.ZERO);
      stakeAddressResponse.setRewardWithdrawn(stakeRewardWithdrawn);
      stakeAddressResponse.setRewardAvailable(stakeAvailableReward.subtract(stakeRewardWithdrawn));
      stakeAddressResponse.setTotalStake(
          latestStakeAddressBalance
              .getQuantity()
              .add(stakeAvailableReward)
              .subtract(stakeRewardWithdrawn));
    }

    if (stakeAddressResponse.getRewardAvailable() == null) {
      stakeAddressResponse.setTotalStake(latestStakeAddressBalance.getQuantity());
    } else {
      stakeAddressResponse.setTotalStake(
          latestStakeAddressBalance.getQuantity().add(stakeAddressResponse.getRewardAvailable()));
    }

    StakeDelegationProjection poolData =
        delegationRepository.findPoolDataByAddress(stakeAddress).orElse(null);
    if (poolData != null) {
      DelegationPoolResponse poolResponse =
          DelegationPoolResponse.builder()
              .poolId(poolData.getPoolId())
              .poolName(poolData.getPoolData())
              .tickerName(poolData.getTickerName())
              .logoUrl(poolData.getLogoUrl())
              .iconUrl(poolData.getIconUrl())
              .build();
      stakeAddressResponse.setPool(poolResponse);
    }
    Long txIdRegister = stakeRegistrationRepository.findMaxTxIdByStake(stakeAddress).orElse(0L);
    Long txIdDeregister = stakeDeRegistrationRepository.findMaxTxIdByStake(stakeAddress).orElse(0L);
    if (txIdRegister.compareTo(txIdDeregister) > 0) {
      stakeAddressResponse.setStatus(StakeAddressStatus.ACTIVE);
    } else {
      stakeAddressResponse.setStatus(StakeAddressStatus.DEACTIVATED);
    }
    stakeAddressResponse.setRewardPools(poolUpdateRepository.findPoolByRewardAccount(stakeAddress));
    return stakeAddressResponse;
  }

  @Override
  public BaseFilterResponse<StakeDelegationProjection> getDelegationHistories(
      String stakeKey, Pageable pageable) {
    Page<StakeDelegationProjection> delegations =
        delegationRepository.findDelegationByAddress(stakeKey, pageable);
    return new BaseFilterResponse<>(delegations);
  }

  @Override
  public BaseFilterResponse<StakeHistoryProjection> getStakeHistories(
      String stakeKey, Pageable pageable) {
    StakeAddress stakeAddress =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    List<StakeHistoryProjection> stakeHistoryList =
        stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeAddress);
    stakeHistoryList.addAll(
        stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeAddress));
    stakeHistoryList.sort(
        (o1, o2) -> {
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
    Page<StakeHistoryProjection> page =
        new PageImpl<>(stakeHistoryList.subList(start, end), pageable, stakeHistoryList.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(
      String stakeKey, Pageable pageable) {
    Page<StakeWithdrawalProjection> withdrawalHistories =
        withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable);

    if (withdrawalHistories.isEmpty() && Boolean.FALSE.equals(fetchRewardDataService.useKoios())) {
      return new BaseFilterResponse<>();
    }

    return new BaseFilterResponse<>(withdrawalHistories);
  }

  @Override
  public BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(
      String stakeKey, Pageable pageable) {
    List<StakeInstantaneousRewardsProjection> instantaneousRewards =
        treasuryRepository.getTreasuryByAddress(stakeKey);
    instantaneousRewards.addAll(reserveRepository.getReserveByAddress(stakeKey));
    instantaneousRewards.sort(
        (o1, o2) -> {
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
    Page<StakeInstantaneousRewardsProjection> page =
        new PageImpl<>(
            instantaneousRewards.subList(start, end), pageable, instantaneousRewards.size());
    return new BaseFilterResponse<>(page);
  }

  @Override
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(Pageable pageable) {
    Pageable pageableDouble =
        PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), pageable.getSort());
    var stakeList = stakeAddressRepository.findStakeAddressOrderByBalance(pageableDouble);
    Set<String> stakeAddressList =
        StreamUtil.mapApplySet(stakeList, StakeAddressProjection::getStakeAddress);
    if (!fetchRewardDataService.useKoios()) {
      return new BaseFilterResponse<>();
    }

    if (!fetchRewardDataService.checkRewardAvailable(stakeAddressList.stream().toList())) {
      boolean fetchRewardResponse =
          fetchRewardDataService.fetchReward(stakeAddressList.stream().toList());
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    var stakeIdList = StreamUtil.mapApplySet(stakeList, StakeAddressProjection::getId);
    var stakeWithdrawals = withdrawalRepository.getRewardWithdrawnByAddrIn(stakeIdList);
    var mapStakeWithdrawnByStakeId =
        StreamUtil.toMap(
            stakeWithdrawals,
            StakeWithdrawalProjection::getStakeAddressId,
            StakeWithdrawalProjection::getAmount);
    var stakeTotalRewards = rewardRepository.getTotalRewardByStakeAddressIn(stakeIdList);
    var mapStakeAvailableRewardsByStakeId =
        StreamUtil.toMap(
            stakeTotalRewards,
            StakeRewardProjection::getStakeAddressId,
            StakeRewardProjection::getAmount);
    var poolData = delegationRepository.findPoolDataByAddressIn(stakeAddressList);
    var mapPoolByStakeAddress =
        StreamUtil.toMap(poolData, StakeDelegationProjection::getStakeAddress);

    List<StakeFilterResponse> content = new ArrayList<>();
    for (var stake : stakeList) {
      StakeDelegationProjection delegation = mapPoolByStakeAddress.get(stake.getStakeAddress());
      StakeFilterResponse stakeResponse =
          stakeAddressMapper.fromStakeAddressAndDelegationProjection(stake, delegation);
      stakeResponse.setPoolName(delegation.getPoolData());
      var stakeWithdrawn = mapStakeWithdrawnByStakeId.getOrDefault(stake.getId(), BigInteger.ZERO);
      var stakeTotalReward =
          mapStakeAvailableRewardsByStakeId.getOrDefault(stake.getId(), BigInteger.ZERO);
      var availableReward = stakeTotalReward.subtract(stakeWithdrawn);
      if (availableReward.compareTo(BigInteger.ZERO) < 0) {
        availableReward = BigInteger.ZERO;
      }
      stakeResponse.setBalance(stake.getTotalStake().add(availableReward));
      content.add(stakeResponse);
    }
    content.sort(Comparator.comparing(StakeFilterResponse::getBalance).reversed());
    Page<StakeFilterResponse> pageResponse =
        new PageImpl<>(content, pageable, pageable.getPageSize());
    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  public BaseFilterResponse<AddressFilterResponse> getAddresses(
      String stakeKey, Pageable pageable) {
    Page<AddressFilterResponse> responsePage =
        addressRepository
            .findByStakeAddress(stakeKey, pageable)
            .map(
                addressResponse -> {
                  AddressFilterResponse response = new AddressFilterResponse();
                  response.setAddress(addressResponse.getAddress());
                  response.setBalance(addressResponse.getBalance());
                  return response;
                });

    return new BaseFilterResponse<>(responsePage);
  }

  @Override
  public StakeAnalyticResponse getStakeAnalytics() {
    StakeAnalyticResponse response = new StakeAnalyticResponse();
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElse(0);
    Boolean useKoios = fetchRewardDataService.useKoios();
    BigInteger activeStake = null;
    BigInteger liveStake = null;
    if (Boolean.TRUE.equals(useKoios)) {
      activeStake = poolInfoRepository.getTotalActiveStake(currentEpoch);
      liveStake = poolInfoRepository.getTotalLiveStake(currentEpoch);
    }
    response.setActiveStake(activeStake);
    response.setLiveStake(liveStake);
    return response;
  }

  @Override
  public AddressChartBalanceResponse getStakeBalanceAnalytics(String stakeKey, AnalyticType type) {
    StakeAddress addr =
        stakeAddressRepository
            .findByView(stakeKey)
            .orElseThrow(() -> new NoContentException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);
    AddressChartBalanceResponse response = new AddressChartBalanceResponse();
    List<AddressChartBalanceData> data = new ArrayList<>();

    if (AnalyticType.ONE_DAY.equals(type)) {
      var fromBalance =
          addressTxAmountRepository
              .sumBalanceByStakeAddress(addr.getView(), dates.get(0).toEpochSecond(ZoneOffset.UTC))
              .orElse(BigInteger.ZERO);

      getHighestAndLowestBalance(addr, fromBalance, dates, response);
      // init data for chart
      data.add(new AddressChartBalanceData(dates.get(0), fromBalance));
      for (int i = 1; i < dates.size(); i++) {
        Optional<BigInteger> balance =
            addressTxAmountRepository.getBalanceByStakeAddressAndTime(
                addr.getView(),
                dates.get(i - 1).toEpochSecond(ZoneOffset.UTC),
                dates.get(i).toEpochSecond(ZoneOffset.UTC));
        if (balance.isPresent()) {
          fromBalance = fromBalance.add(balance.get());
        }
        data.add(new AddressChartBalanceData(dates.get(i), fromBalance));
      }
      response.setData(data);
    } else {
      // Remove last date because we will get data of today
      dates.remove(0);
      var fromBalance =
          addressTxAmountRepository
              .sumBalanceByStakeAddress(addr.getView(), dates.get(0).toEpochSecond(ZoneOffset.UTC))
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);
      List<AggregateAddressBalanceProjection> aggregateAddressTxBalances =
          aggregateAddressTxBalanceRepository.findAllByStakeAddressIdAndDayBetween(
              addr.getId(), dates.get(0).toLocalDate(), dates.get(dates.size() - 1).toLocalDate());
      // Data in aggregate_address_tx_balance save at end of day, but we will display start of day
      // So we need to add 1 day to display correct data
      Map<LocalDate, BigInteger> mapBalance =
          aggregateAddressTxBalances.stream()
              .collect(
                  Collectors.toMap(
                      balance -> balance.getDay().plusDays(1),
                      AggregateAddressBalanceProjection::getBalance));
      for (LocalDateTime date : dates) {
        if (mapBalance.containsKey(date.toLocalDate())) {
          fromBalance = fromBalance.add(mapBalance.get(date.toLocalDate()));
        }
        data.add(new AddressChartBalanceData(date, fromBalance));
      }
      response.setData(data);
    }
    return response;
  }

  /**
   * Get highest and lowest balance of stake address
   *
   * @param addr stake address
   * @param fromBalance balance of stake address at start date
   * @param dates list date
   * @param response chart response
   */
  private void getHighestAndLowestBalance(
      StakeAddress addr,
      BigInteger fromBalance,
      List<LocalDateTime> dates,
      AddressChartBalanceResponse response) {
    var minMaxBalance =
        stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(
            addr.getId(),
            fromBalance,
            dates.get(0).toEpochSecond(ZoneOffset.UTC),
            dates.get(dates.size() - 1).toEpochSecond(ZoneOffset.UTC));
    if (minMaxBalance.getMaxVal().compareTo(fromBalance) > 0) {
      response.setHighestBalance(minMaxBalance.getMaxVal());
    } else {
      response.setHighestBalance(fromBalance);
    }
    if (minMaxBalance.getMinVal().compareTo(fromBalance) < 0) {
      response.setLowestBalance(minMaxBalance.getMinVal());
    } else {
      response.setLowestBalance(fromBalance);
    }
    Long maxTxId =
        stakeTxBalanceRepository.findMaxTxIdByStakeAddressId(addr.getId()).orElse(Long.MAX_VALUE);
    if (!maxTxId.equals(Long.MAX_VALUE)) {
      List<StakeTxProjection> stakeTxList =
          addressTxAmountRepository.findTxAndAmountByStake(addr.getView(), maxTxId);
      for (StakeTxProjection stakeTx : stakeTxList) {
        if (response
                .getHighestBalance()
                .add(stakeTx.getAmount())
                .compareTo(response.getHighestBalance())
            > 0) {
          response.setHighestBalance(response.getHighestBalance().add(stakeTx.getAmount()));
        }
        if (response
                .getLowestBalance()
                .add(stakeTx.getAmount())
                .compareTo(response.getLowestBalance())
            < 0) {
          response.setLowestBalance(response.getLowestBalance().add(stakeTx.getAmount()));
        }
      }
    }
  }

  @Override
  public List<StakeAnalyticRewardResponse> getStakeRewardAnalytics(String stakeKey) {
    if (Boolean.FALSE.equals(fetchRewardDataService.useKoios())) {
      return null;
    }

    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    List<StakeAnalyticRewardResponse> responses = rewardRepository.findRewardByStake(stakeKey);
    Map<Integer, BigInteger> rewardMap =
        responses.stream()
            .collect(
                Collectors.toMap(
                    StakeAnalyticRewardResponse::getEpoch, StakeAnalyticRewardResponse::getValue));
    responses = new ArrayList<>();
    int startEpoch = rewardMap.keySet().stream().mapToInt(v -> v).min().orElse(0);
    int currentEpoch = epochRepository.findCurrentEpochNo().orElse(2) - 2;
    for (int epoch = startEpoch; epoch <= currentEpoch; epoch++) {
      StakeAnalyticRewardResponse response;
      response =
          new StakeAnalyticRewardResponse(epoch, rewardMap.getOrDefault(epoch, BigInteger.ZERO));
      responses.add(response);
    }
    return responses;
  }

  @Override
  public StakeAddressRewardDistribution getStakeAddressRewardDistributionInfo(String stakeKey) {
    StakeAddressRewardDistribution stakeAddressRewardDistribution =
        new StakeAddressRewardDistribution();
    stakeAddressRewardDistribution.setStakeAddress(stakeKey);
    if (Boolean.FALSE.equals(fetchRewardDataService.useKoios())) {
      return stakeAddressRewardDistribution;
    }

    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
    BigInteger stakeAvailableReward =
        rewardRepository.getAvailableRewardByStakeAddress(stakeKey).orElse(BigInteger.ZERO);
    stakeAddressRewardDistribution.setRewardAvailable(stakeAvailableReward);
    Set<RewardType> rewardTypeOfStakeKey = rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey);
    if (rewardTypeOfStakeKey.contains(RewardType.MEMBER)) {
      stakeAddressRewardDistribution.setHasMemberReward(true);
    }
    if (rewardTypeOfStakeKey.contains(RewardType.LEADER)) {
      stakeAddressRewardDistribution.setHasLeaderReward(true);
    }
    return stakeAddressRewardDistribution;
  }
}
