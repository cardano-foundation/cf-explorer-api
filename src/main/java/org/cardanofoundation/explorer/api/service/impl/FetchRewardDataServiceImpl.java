package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.service.FetchRewardDataService;

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
}
