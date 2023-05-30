package org.cardanofoundation.explorer.api.service;

public interface FetchRewardDataService {

  boolean checkRewardAvailable(String stakeKey);

  Boolean fetchReward(String stakeKey);

}
