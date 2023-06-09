package org.cardanofoundation.explorer.api.service.impl;

import java.util.List;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
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
  public Boolean checkPoolHistoryForPool(Set<String> poolIds) {
    return true;
  }

  @Override
  public Boolean checkPoolInfoForPool(Set<String> poolIds) {
    return true;
  }

  @Override
  public Boolean fetchPoolHistoryForPool(Set<String> poolIds) {
    return true;
  }

  @Override
  public Boolean fetchPoolInfoForPool(Set<String> poolIds) {
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
  public Boolean isKoiOs() {
    return false;
  }
}
