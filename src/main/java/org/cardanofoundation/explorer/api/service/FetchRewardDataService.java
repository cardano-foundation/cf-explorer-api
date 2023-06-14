package org.cardanofoundation.explorer.api.service;

import java.util.List;
import java.util.Set;

public interface FetchRewardDataService {

  boolean checkRewardAvailable(String stakeKey);

  Boolean fetchReward(String stakeKey);

  Boolean checkPoolHistoryForPool(Set<String> poolIds);

  Set<String> checkAllPoolHistoryForPool(Set<String> poolIds);

  Boolean fetchPoolHistoryForPool(Set<String> poolIds);

  Boolean checkPoolInfoForPool(Set<String> poolIds);

  Set<String> checkAllPoolInfoForPool();

  Boolean fetchPoolInfoForPool(Set<String> poolIds);

  Boolean checkRewardForPool(List<String> rewardAccounts);

  Boolean fetchRewardForPool(List<String> rewardAccounts);

  Boolean checkEpochStakeForPool(List<String> rewardAccounts);

  Boolean fetchEpochStakeForPool(List<String> rewardAccounts);

  Boolean checkAdaPots(Integer epochNo);

  Boolean fetchAdaPots(List<Integer> epochNo);

  Boolean isKoiOs();
}
