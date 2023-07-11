package org.cardanofoundation.explorer.api.service.impl;

import java.util.*;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.repository.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.EpochStakeCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.PoolInfoCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

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

  @Value("${application.api.check-epoch-stake.base-url}")
  private String apiCheckEpochStakeUrl;

  @Value("${application.api.check-ada-pots.base-url}")
  private String apiCheckAdaPotsUrl;

  @Value("${application.api.check-epoch.base-url}")
  private String apiCheckEpochUrl;

  private final RestTemplate restTemplate;
  private final RewardCheckpointRepository rewardCheckpointRepository;

  private final PoolHistoryCheckpointRepository poolHistoryCheckpointRepository;

  private final PoolInfoCheckpointRepository poolInfoCheckpointRepository;

  private final EpochStakeCheckpointRepository epochStakeCheckpointRepository;

  private final AdaPotsRepository adaPotsRepository;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return restTemplate.postForObject(apiCheckRewardUrl, Collections.singleton(stakeKey),
        Boolean.class);
  }

  @Override
  public Boolean checkRewardAvailable(List<String> stakeAddressList) {
    Integer countCheckPoint = rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(
        stakeAddressList);
    Integer sizeCheck = stakeAddressList.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchReward(List<String> stakeAddressList) {
    return restTemplate.postForObject(apiCheckRewardUrl, stakeAddressList,
        Boolean.class);
  }

  @Override
  public Boolean checkPoolHistoryForPool(Set<String> poolIds) {
    Integer countCheckPoint = poolHistoryCheckpointRepository.checkRewardByPoolViewAndEpoch(
        poolIds);
    Integer sizeCheck = poolIds.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Set<String> checkAllPoolHistoryForPool(Set<String> poolIds) {
    return poolHistoryCheckpointRepository.checkPoolHistoryByPoolViewAndEpoch(poolIds);
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
  public Set<String> checkAllPoolInfoForPool() {
    return poolInfoCheckpointRepository.checkPoolInfoByPoolViewAndEpoch();
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
  public Boolean checkEpochStakeForPool(List<String> rewardAccounts) {
    Integer countCheckPoint = epochStakeCheckpointRepository.checkEpochStakeByAccountsAndEpoch(
        rewardAccounts);
    Integer sizeCheck = rewardAccounts.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchEpochStakeForPool(List<String> rewardAccounts) {
    return restTemplate.postForObject(apiCheckEpochStakeUrl, rewardAccounts,
        Boolean.class);
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
  public Boolean checkAdaPots(Integer epochNo) {
    return adaPotsRepository.existsByEpochNo(epochNo);
  }

  @Override
  public Boolean fetchAdaPots(List<Integer> epochNo) {
    return restTemplate.postForObject(apiCheckAdaPotsUrl, epochNo,
        Boolean.class);
  }

  @Override
  public Boolean checkEpochRewardDistributed(Epoch epoch) {
    return Objects.nonNull(epoch.getRewardsDistributed());
  }

  @Override
  public List<Epoch> fetchEpochRewardDistributed(List<Integer> epochNoList) {
    Epoch[] epoch = restTemplate.postForObject(apiCheckEpochUrl, epochNoList,
        Epoch[].class);
    if (Objects.nonNull(epoch)) {
      return Arrays.asList(epoch);
    }
    return Collections.emptyList();
  }

  @Override
  public Boolean isKoiOs() {
    return true;
  }
}
