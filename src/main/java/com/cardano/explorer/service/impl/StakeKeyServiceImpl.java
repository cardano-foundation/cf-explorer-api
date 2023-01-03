package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.StakeAddressMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.DelegationPoolResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.cardano.explorer.projection.StakeAddressProjection;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeTreasuryProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
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
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyServiceImpl implements StakeKeyService {

  private final DelegationRepository delegationRepository;

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;

  private final PoolOfflineDataRepository poolOfflineDataRepository;

  private final PoolHashRepository poolHashRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RewardRepository rewardRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final TreasuryRepository treasuryRepository;
  private final StakeAddressMapper stakeAddressMapper;

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeRegistrationRepository.getDataForStakeRegistration(pageable);
    return createStakeKeyResponse(trxBlockEpochStakePage, pageable);
  }

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeDeRegistrationRepository.getDataForStakeDeRegistration(pageable);
    return createStakeKeyResponse(trxBlockEpochStakePage, pageable);
  }

  private BaseFilterResponse<StakeTxResponse> createStakeKeyResponse(Page<TrxBlockEpochStake> page, Pageable pageable){
    BaseFilterResponse<StakeTxResponse> response = new BaseFilterResponse<>();
    List<StakeTxResponse> responseList = page.stream().map(StakeTxResponse::new)
            .collect(Collectors.toList());
    response.setData(responseList);
    response.setTotalItems(page.getTotalElements());
    response.setTotalPages(page.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
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
    BigDecimal stakeTotalBalance = stakeAddress.getBalance();
    BigDecimal stakeRewardWithdrawn = withdrawalRepository.getRewardWithdrawnByStakeAddress(
        stake).orElse(BigDecimal.ZERO);
    BigDecimal stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
        stake).orElse(BigDecimal.ZERO);
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
    return stakeAddressResponse;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeDelegationProjection> getDelegationHistories(String stakeKey, Pageable pageable) {
    BaseFilterResponse<StakeDelegationProjection> response = new BaseFilterResponse<>();
    Page<StakeDelegationProjection> delegations
        = delegationRepository.findDelegationByAddress(stakeKey, pageable);
    response.setData(delegations.getContent());
    response.setTotalPages(delegations.getTotalPages());
    response.setTotalItems(delegations.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeHistoryProjection> getStakeHistories(String stakeKey, Pageable pageable) {
    BaseFilterResponse<StakeHistoryProjection> response = new BaseFilterResponse<>();
    Page<StakeHistoryProjection> stakeHistories
        = stakeAddressRepository.getStakeHistory(stakeKey, pageable);
    response.setData(stakeHistories.getContent());
    response.setTotalPages(stakeHistories.getTotalPages());
    response.setTotalItems(stakeHistories.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(String stakeKey, Pageable pageable) {
    BaseFilterResponse<StakeWithdrawalProjection> response = new BaseFilterResponse<>();
    Page<StakeWithdrawalProjection> withdrawalHistories
        = withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable);
    response.setData(withdrawalHistories.getContent());
    response.setTotalPages(withdrawalHistories.getTotalPages());
    response.setTotalItems(withdrawalHistories.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeTreasuryProjection> getInstantaneousRewards(String stakeKey, Pageable pageable) {
    BaseFilterResponse<StakeTreasuryProjection> response = new BaseFilterResponse<>();
    Page<StakeTreasuryProjection> instantaneousRewards
        = treasuryRepository.getTreasuryByAddress(stakeKey, pageable);
    response.setData(instantaneousRewards.getContent());
    response.setTotalPages(instantaneousRewards.getTotalPages());
    response.setTotalItems(instantaneousRewards.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(Pageable pageable) {
    BaseFilterResponse<StakeFilterResponse> response = new BaseFilterResponse<>();
    Page<StakeAddressProjection> stakePage
        = stakeAddressRepository.findStakeAddressOrderByBalance(pageable);

    List<StakeFilterResponse> content = new ArrayList<>();
    Set<String> stakeAddressList = stakePage.getContent().stream()
        .map(StakeAddressProjection::getStakeAddress).collect(Collectors.toSet());
    var poolData = delegationRepository.findPoolDataByAddressIn(stakeAddressList);
    var poolDataMap = poolData.stream().collect(Collectors.toMap(
        StakeDelegationProjection::getStakeAddress, Function.identity()));
    for(var stake : stakePage) {
      StakeDelegationProjection delegation =  poolDataMap.get(stake.getStakeAddress());
      StakeFilterResponse stakeResponse
          = stakeAddressMapper.fromStakeAddressAndDelegationProjection(stake, delegation);
      stakeResponse.setPoolName(getNameValueFromJson(delegation.getPoolData()));
      content.add(stakeResponse);
    }
    response.setData(content);
    response.setTotalPages(stakePage.getTotalPages());
    response.setTotalItems(stakePage.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
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
