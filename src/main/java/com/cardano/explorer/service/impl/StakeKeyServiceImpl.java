package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AddressMapper;
import com.cardano.explorer.mapper.StakeAddressMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.StakeAnalyticResponse;
import com.cardano.explorer.model.response.address.AddressFilterResponse;
import com.cardano.explorer.model.response.address.DelegationPoolResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.cardano.explorer.projection.StakeAddressProjection;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeInstantaneousRewardsProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.ReserveRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.repository.TreasuryRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.StakeKeyService;
import com.cardano.explorer.util.AddressUtils;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
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
  private final StakeAddressMapper stakeAddressMapper;
  private final AddressMapper addressMapper;
  private final EpochRepository epochRepository;
  private final EpochStakeRepository epochStakeRepository;

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
          .poolName(getNameValueFromJson(poolData.getPoolData()))
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
    List<StakeHistoryProjection> stakeHistoryList =
        stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeKey);
    stakeHistoryList.addAll(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeKey));
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
    List<StakeAddressProjection> stakeList
        = stakeAddressRepository.findStakeAddressOrderByBalance(pageable);
    List<StakeFilterResponse> content = new ArrayList<>();
    Set<String> stakeAddressList = stakeList.stream()
        .map(StakeAddressProjection::getStakeAddress).collect(Collectors.toSet());
    var poolData = delegationRepository.findPoolDataByAddressIn(stakeAddressList);
    var poolDataMap = poolData.stream().collect(Collectors.toMap(
        StakeDelegationProjection::getStakeAddress, Function.identity()));
    for(var stake : stakeList) {
      StakeDelegationProjection delegation =  poolDataMap.get(stake.getStakeAddress());
      StakeFilterResponse stakeResponse
          = stakeAddressMapper.fromStakeAddressAndDelegationProjection(stake, delegation);
      stakeResponse.setPoolName(getNameValueFromJson(delegation.getPoolData()));
      content.add(stakeResponse);
    }
    Page<StakeFilterResponse> pageResponse
        = new PageImpl<>(content, pageable, pageable.getPageSize());
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
    response.setLiveStake(epochStakeRepository.totalStakeAllPoolByEpochNo(currentEpoch).orElse(BigInteger.ZERO));
    if(1 > currentEpoch) {
      response.setActiveStake(BigInteger.ZERO);
    } else {
      response.setActiveStake(epochStakeRepository.totalStakeAllPoolByEpochNo(currentEpoch - 1).orElse(BigInteger.ZERO));
    }
    return response;
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      return null;
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }

}
