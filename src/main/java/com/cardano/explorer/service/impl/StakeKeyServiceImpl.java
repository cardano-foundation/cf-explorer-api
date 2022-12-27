package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.DelegationPoolResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeTreasuryProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.repository.TreasuryRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.AddressService;
import com.cardano.explorer.service.StakeKeyService;
import com.cardano.explorer.util.AddressUtils;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
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

  private final AddressRepository addressRepository;

  private final DelegationRepository delegationRepository;

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;

  private final PoolOfflineDataRepository poolOfflineDataRepository;

  private final PoolHashRepository poolHashRepository;

  private final TxOutRepository txOutRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RewardRepository rewardRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final TreasuryRepository treasuryRepository;
  private final AddressService addressService;

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
    List<Long> blockIdList = responseList.stream().map(StakeTxResponse::getBlock).collect(Collectors.toList());
    List<Long> poolIds = poolHashRepository.getListPoolIdIn(blockIdList);
    if(!poolIds.isEmpty()){
      List<PoolOfflineData> listAllPoolOfflineName = poolOfflineDataRepository.findAllByListPool(poolIds);
      // map with key: pool id, value: list pool offline
      Map<Long, List<PoolOfflineData>> mapPoolIdPoolName = listAllPoolOfflineName.stream()
              .collect(Collectors.groupingBy(item -> item.getPool().getId(), Collectors.toList()));
      for(int i = 0; i < responseList.size(); i++){
        if(poolIds.size() > i){
          responseList.get(i).setPoolNames(mapPoolIdPoolName
                  .get(poolIds.get(i))
                  .stream()
                  .map(item -> getNameValueFromJson(item.getJson()))
                  .collect(Collectors.toList()));
        }
      }
    }
    response.setData(responseList);
    response.setTotalItems(page.getTotalElements());
    response.setTotalPages(page.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

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
    BigDecimal stakeTotalBalance = addressRepository.getBalanceByStakeAddress(stakeAddress)
        .orElse(BigDecimal.ZERO);
    BigDecimal stakeRewardWithdrawn = withdrawalRepository.getRewardWithdrawnByStakeAddress(
        stake).orElse(BigDecimal.ZERO);
    BigDecimal stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
        stake).orElse(BigDecimal.ZERO);
    stakeAddressResponse.setRewardWithdrawn(stakeRewardWithdrawn);
    stakeAddressResponse.setRewardAvailable(stakeAvailableReward.subtract(stakeRewardWithdrawn));
    stakeAddressResponse.setTotalStake(stakeTotalBalance.add(stakeAvailableReward)
            .subtract(stakeRewardWithdrawn));
    PoolOfflineData poolData = poolOfflineDataRepository.findPoolDataByAddress(stakeAddress)
        .orElse(null);
    if (poolData != null) {
      DelegationPoolResponse poolResponse = DelegationPoolResponse.builder()
          .poolId(poolData.getPool().getView())
          .poolName(getNameValueFromJson(poolData.getJson()))
          .tickerName(poolData.getTickerName())
          .build();
      stakeAddressResponse.setStatus(StakeAddressStatus.ACTIVE);
      stakeAddressResponse.setPool(poolResponse);
    } else {
      stakeAddressResponse.setStatus(StakeAddressStatus.DEACTIVATED);
      stakeAddressResponse.setPool(null);
    }
    return stakeAddressResponse;
  }

  @Override
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

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }

}
