package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.response.address.DelegationPoolResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.StakeAddressService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardano.ledgersync.common.address.ShelleyAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class StakeAddressServiceImpl implements StakeAddressService {

  private final TxOutRepository txOutRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final RewardRepository rewardRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final PoolOfflineDataRepository poolOfflineDataRepository;

  @Override
  @Transactional(readOnly = true)
  public StakeAddressResponse getStakeAddress(String address) {
    StakeAddressResponse stakeAddressResponse = new StakeAddressResponse();
    try {
      ShelleyAddress shelleyAddress = new ShelleyAddress(address);
      if(!shelleyAddress.containStakeAddress()){
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
      //TO-DO
      byte[] addr = shelleyAddress.getStakeReference();
      ShelleyAddress stake = new ShelleyAddress(addr);
      StakeAddress stakeAddress
          = stakeAddressRepository.findByView(stake.getAddress()).orElseThrow(
          () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
      stakeAddressResponse.setStakeAddress(stake.getAddress());
      BigDecimal stakeTotalOutput = txOutRepository.getStakeAddressTotalOutput(stake.getAddress())
          .orElse(BigDecimal.ZERO);
      BigDecimal stakeTotalInput = txOutRepository.getStakeAddressTotalInput(stake.getAddress())
          .orElse(BigDecimal.ZERO);
      BigDecimal stakeRewardWithDrawn = withdrawalRepository.getRewardWithdrawnByStakeAddress(
          stake.getAddress()).orElse(BigDecimal.ZERO);
      BigDecimal stakeAvailableReward = rewardRepository.getAvailableRewardByStakeAddress(
          stake.getAddress()).orElse(BigDecimal.ZERO);
      stakeAddressResponse.setRewardWithdrawn(stakeRewardWithDrawn);
      stakeAddressResponse.setRewardAvailable(stakeAvailableReward.subtract(stakeRewardWithDrawn));
      stakeAddressResponse.setTotalStake(
          stakeTotalOutput.subtract(stakeTotalInput).add(stakeAvailableReward)
              .subtract(stakeRewardWithDrawn));
      PoolOfflineData poolData = poolOfflineDataRepository.findPoolDataByAddress(stakeAddress)
          .orElse(null);
      if (poolData != null) {
        JsonObject jsonObject = new Gson().fromJson(poolData.getJson(), JsonObject.class);
        DelegationPoolResponse poolResponse = DelegationPoolResponse.builder()
            .poolId(poolData.getPool().getView())
            .poolName(jsonObject.get("name").getAsString())
            .tickerName(jsonObject.get("ticker").getAsString())
            .build();
        stakeAddressResponse.setStatus(StakeAddressStatus.ACTIVE);
        stakeAddressResponse.setPool(poolResponse);
      } else {
        stakeAddressResponse.setStatus(StakeAddressStatus.INACTIVE);
        stakeAddressResponse.setPool(null);
      }
      return stakeAddressResponse;
    } catch (Exception e) {
        throw new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND);
    }

  }
}
