package org.cardanofoundation.explorer.api.service.impl;

import java.util.*;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Profile("!koios")
@Service
@RequiredArgsConstructor
public class FetchRewardDataServiceImpl implements FetchRewardDataService {

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return true;
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return true;
  }

  @Override
  public Boolean checkRewardAvailable(List<String> stakeAddressList) {
    return true;
  }

  @Override
  public Boolean fetchReward(List<String> stakeAddressList) {
    return true;
  }

  @Override
  public Boolean checkPoolHistoryForPool(Set<String> poolIds) {
    return true;
  }

  @Override
  public Boolean fetchPoolHistoryForPool(Set<String> poolIds) {
    return true;
  }

  @Override
  public Boolean checkRewardForPool(List<String> rewardAccounts) {
    return true;
  }

  @Override
  public Boolean fetchRewardForPool(List<String> rewardAccounts) {
    return true;
  }

  @Override
  public Boolean checkEpochStakeForPool(List<String> rewardAccounts) {
    return true;
  }

  @Override
  public Boolean fetchEpochStakeForPool(List<String> rewardAccounts) {
    return true;
  }

  @Override
  public Boolean checkAdaPots(Integer epochNo) {
    return true;
  }

  @Override
  public Boolean fetchAdaPots(List<Integer> epochNo) {
    return true;
  }

  @Override
  public Boolean checkEpochRewardDistributed(Epoch epoch) {
    return true;
  }

  @Override
  public List<Epoch> fetchEpochRewardDistributed(List<Integer> epochNo) {
    return Collections.emptyList();
  }

  @Override
  public Boolean useKoios() {
    return false;
  }
}
