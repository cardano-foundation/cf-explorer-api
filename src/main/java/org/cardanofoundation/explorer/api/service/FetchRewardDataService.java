package org.cardanofoundation.explorer.api.service;

import java.util.List;
import java.util.Set;

import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;

public interface FetchRewardDataService {

  boolean checkRewardAvailable(String stakeKey);

  Boolean fetchReward(String stakeKey);

  Boolean checkRewardAvailable(List<String> stakeAddressList);

  Boolean fetchReward(List<String> stakeAddressList);

  Boolean checkPoolHistoryForPool(Set<String> poolIds);

  Boolean fetchPoolHistoryForPool(Set<String> poolIds);

  Boolean checkRewardForPool(List<String> rewardAccounts);

  Boolean fetchRewardForPool(List<String> rewardAccounts);

  Boolean checkEpochStakeForPool(List<String> rewardAccounts);

  Boolean fetchEpochStakeForPool(List<String> rewardAccounts);

  Boolean checkAdaPots(Integer epochNo);

  Boolean fetchAdaPots(List<Integer> epochNo);

  Boolean checkEpochRewardDistributed(Epoch epoch);

  List<Epoch> fetchEpochRewardDistributed(List<Integer> epochNo);

  Boolean useKoios();
}
