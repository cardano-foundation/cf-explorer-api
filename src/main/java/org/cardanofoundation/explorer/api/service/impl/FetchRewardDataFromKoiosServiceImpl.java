package org.cardanofoundation.explorer.api.service.impl;

import java.util.*;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolInfoCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.WebClientService;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.enumeration.EraType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

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

  private final RewardCheckpointRepository rewardCheckpointRepository;

  private final PoolHistoryCheckpointRepository poolHistoryCheckpointRepository;

  private final PoolInfoCheckpointRepository poolInfoCheckpointRepository;

  private final EpochStakeCheckpointRepository epochStakeCheckpointRepository;

  private final AdaPotsRepository adaPotsRepository;

  private final WebClientService webClientService;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return webClientService.postWebClient(apiCheckRewardUrl,Boolean.class,stakeKey).block();
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
    return webClientService.postWebClient(apiCheckRewardUrl,Boolean.class,stakeAddressList).block();
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
          webClientService.postWebClient(apiCheckPoolHistoryUrl, Boolean.class, poolIds).block());
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
          webClientService.postWebClient(apiCheckPoolInfoUrl, Boolean.class, poolIds).block());
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
    return webClientService.postWebClient(apiCheckEpochStakeUrl, Boolean.class,rewardAccounts).block();
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
    return webClientService.postWebClient(apiCheckRewardUrl,Boolean.class,rewardAccounts).block();
  }

  @Override
  public Boolean checkAdaPots(Integer epochNo) {
    return adaPotsRepository.existsByEpochNo(epochNo);
  }

  @Override
  public Boolean fetchAdaPots(List<Integer> epochNo) {
    return webClientService.postWebClient(apiCheckAdaPotsUrl,Boolean.class,epochNo).block();
  }

  @Override
  public Boolean checkEpochRewardDistributed(Epoch epoch) {
    return Objects.nonNull(epoch.getRewardsDistributed())
        || epoch.getEra().equals(EraType.BYRON)
        || epoch.getEra().equals(EraType.BYRON_EBB);
  }

  @Override
  public List<Epoch> fetchEpochRewardDistributed(List<Integer> epochNoList) {
    try {
      Epoch[] epoch = webClientService.postWebClient(apiCheckEpochUrl, Epoch[].class, epochNoList).block();
      if (Objects.nonNull(epoch)) {
        return Arrays.asList(epoch);
      }
      return Collections.emptyList();
    } catch (Exception e) {
      return null;
    }
  }

  @Override
  public Boolean useKoios() {
    return true;
  }
}
