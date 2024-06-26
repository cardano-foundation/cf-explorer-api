package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.util.*;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import reactor.core.publisher.Mono;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.entity.enumeration.EraType;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Profile("koios")
@Service
@RequiredArgsConstructor
public class FetchRewardDataFromKoiosServiceImpl implements FetchRewardDataService {

  @Value("${application.api.check-reward.base-url}")
  private String apiCheckRewardUrl;

  @Value("${application.api.check-pool-history.base-url}")
  private String apiCheckPoolHistoryUrl;

  @Value("${application.api.check-epoch-stake.base-url}")
  private String apiCheckEpochStakeUrl;

  @Value("${application.api.check-ada-pots.base-url}")
  private String apiCheckAdaPotsUrl;

  @Value("${application.api.check-epoch.base-url}")
  private String apiCheckEpochUrl;

  private final RewardCheckpointRepository rewardCheckpointRepository;

  private final PoolHistoryCheckpointRepository poolHistoryCheckpointRepository;

  private final EpochStakeCheckpointRepository epochStakeCheckpointRepository;

  private final AdaPotsRepository adaPotsRepository;

  private final WebClient webClient;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return postWebClient(apiCheckRewardUrl, Boolean.class, Collections.singleton(stakeKey)).block();
  }

  @Override
  public Boolean checkRewardAvailable(List<String> stakeAddressList) {
    Integer countCheckPoint =
        rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(stakeAddressList);
    Integer sizeCheck = stakeAddressList.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchReward(List<String> stakeAddressList) {
    return postWebClient(apiCheckRewardUrl, Boolean.class, stakeAddressList).block();
  }

  @Override
  public Boolean checkPoolHistoryForPool(Set<String> poolIds) {
    Integer countCheckPoint =
        poolHistoryCheckpointRepository.checkRewardByPoolViewAndEpoch(poolIds);
    Integer sizeCheck = poolIds.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchPoolHistoryForPool(Set<String> poolIds) {
    return postWebClient(apiCheckPoolHistoryUrl, Boolean.class, poolIds).block();
  }

  @Override
  public Boolean checkEpochStakeForPool(List<String> rewardAccounts) {
    Integer countCheckPoint =
        epochStakeCheckpointRepository.checkEpochStakeByAccountsAndEpoch(rewardAccounts);
    Integer sizeCheck = rewardAccounts.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchEpochStakeForPool(List<String> rewardAccounts) {
    return postWebClient(apiCheckEpochStakeUrl, Boolean.class, rewardAccounts).block();
  }

  @Override
  public Boolean checkRewardForPool(List<String> rewardAccounts) {
    Integer countCheckPoint =
        rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(rewardAccounts);
    Integer sizeCheck = rewardAccounts.size();
    return Objects.equals(countCheckPoint, sizeCheck);
  }

  @Override
  public Boolean fetchRewardForPool(List<String> rewardAccounts) {
    return postWebClient(apiCheckRewardUrl, Boolean.class, rewardAccounts).block();
  }

  @Override
  public Boolean checkAdaPots(Integer epochNo) {
    return adaPotsRepository.existsByEpochNo(epochNo);
  }

  @Override
  public Boolean fetchAdaPots(List<Integer> epochNo) {
    return postWebClient(apiCheckAdaPotsUrl, Boolean.class, epochNo).block();
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
      Epoch[] epoch = postWebClient(apiCheckEpochUrl, Epoch[].class, epochNoList).block();
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

  public <T> Mono<T> postWebClient(String url, Class<T> clazz, Object body) {
    return webClient
        .post()
        .uri(url)
        .bodyValue(body)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(
            status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(new BusinessException(BusinessCode.FETCH_REWARD_ERROR)))
        .bodyToMono(clazz);
  }
}
