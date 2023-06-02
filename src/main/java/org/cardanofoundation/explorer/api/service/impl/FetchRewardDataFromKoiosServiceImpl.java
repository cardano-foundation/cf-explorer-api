package org.cardanofoundation.explorer.api.service.impl;

import java.util.Collections;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.api.repository.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.PoolInfoCheckpointRepository;
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

  @Value("${application.api.check-pool-history.base-url}")
  private String apiCheckPoolHistoryUrl;

  @Value("${application.api.check-pool-info.base-url}")
  private String apiCheckPoolInfoUrl;

  private final RestTemplate restTemplate;
  private final RewardCheckpointRepository rewardCheckpointRepository;

  private final PoolHistoryCheckpointRepository poolHistoryCheckpointRepository;

  private final PoolInfoCheckpointRepository poolInfoCheckpointRepository;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return restTemplate.postForObject(apiCheckRewardUrl, Collections.singleton(stakeKey), Boolean.class);
  }

  @Override
  public Boolean checkPoolHistoryForPool(Set<String> poolIds) {
    Integer countCheckPoint = poolHistoryCheckpointRepository.checkRewardByPoolViewAndEpoch(
        poolIds);
    Integer sizeCheck = poolIds.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchPoolHistoryForPool(Set<String> poolIds) {
    int i = 0;
    boolean isFetch = false;
    while (i < 2) {
      isFetch = Boolean.TRUE.equals(
          restTemplate.postForObject(apiCheckPoolHistoryUrl, poolIds, Boolean.class));
      if (isFetch) {
        break;
      }
      i++;
    }
    return isFetch;
  }

  @Override
  public Boolean checkPoolInfoForPool(Set<String> poolIds) {
    Integer countCheckPoint = poolInfoCheckpointRepository.checkRewardByPoolViewAndEpoch(
        poolIds);
    Integer sizeCheck = poolIds.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchPoolInfoForPool(Set<String> poolIds) {
    int i = 0;
    boolean isFetch = false;
    while (i < 2) {
      isFetch = Boolean.TRUE.equals(
          restTemplate.postForObject(apiCheckPoolInfoUrl, poolIds, Boolean.class));
      if (isFetch) {
        break;
      }
      i++;
    }
    return isFetch;
  }

  @Override
  public Boolean checkRewardForPool(List<String> rewardAccounts) {
    Integer countCheckPoint = rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(
        rewardAccounts);
    Integer sizeCheck = rewardAccounts.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchRewardForPool(List<String> rewardAccounts) {
    return restTemplate.postForObject(apiCheckRewardUrl, rewardAccounts,
        Boolean.class);
  }

  @Override
  public Boolean isKoiOs() {
    return true;
  }
}
