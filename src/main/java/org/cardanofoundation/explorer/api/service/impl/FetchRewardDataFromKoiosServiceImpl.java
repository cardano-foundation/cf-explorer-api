package org.cardanofoundation.explorer.api.service.impl;

import java.util.Collections;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import org.cardanofoundation.explorer.api.repository.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;

@Profile("koios")
@Service
@RequiredArgsConstructor
public class FetchRewardDataFromKoiosServiceImpl implements FetchRewardDataService {

  @Value("${application.api.check-reward.base-url}")
  private String apiCheckRewardUrl;

  private final RestTemplate restTemplate;
  private final RewardCheckpointRepository rewardCheckpointRepository;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return restTemplate.postForObject(apiCheckRewardUrl, Collections.singleton(stakeKey), Boolean.class);
  }
}
