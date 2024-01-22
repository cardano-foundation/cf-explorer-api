package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.util.*;

import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolInfoCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.enumeration.EraType;
import reactor.core.publisher.Mono;

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

  private final WebClient webClient;

  @Override
  public boolean checkRewardAvailable(String stakeKey) {
    return rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(stakeKey);
  }

  @Override
  public Boolean fetchReward(String stakeKey) {
    return webClient.post()
        .uri(apiCheckRewardUrl)
        .acceptCharset(StandardCharsets.UTF_8)
        .bodyValue(Collections.singleton(stakeKey))
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Boolean.class)
        .block();
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
    return webClient.post()
        .uri(apiCheckRewardUrl)
        .acceptCharset(StandardCharsets.UTF_8)
        .bodyValue(stakeAddressList)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Boolean.class)
        .block();
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
          webClient.post()
              .uri(apiCheckPoolHistoryUrl)
              .acceptCharset(StandardCharsets.UTF_8)
              .bodyValue(poolIds)
              .retrieve()
              .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
                  clientResponse -> Mono.error(
                      new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
              .bodyToMono(Boolean.class)
              .block()
          );
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
          webClient.post()
              .uri(apiCheckPoolInfoUrl)
              .acceptCharset(StandardCharsets.UTF_8)
              .bodyValue(poolIds)
              .retrieve()
              .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
                  clientResponse -> Mono.error(
                      new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
              .bodyToMono(Boolean.class)
              .block());
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
    return webClient.post()
        .uri(apiCheckEpochStakeUrl)
        .acceptCharset(StandardCharsets.UTF_8)
        .bodyValue(rewardAccounts)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Boolean.class)
        .block();
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
    return webClient.post()
        .uri(apiCheckRewardUrl)
        .acceptCharset(StandardCharsets.UTF_8)
        .bodyValue(rewardAccounts)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Boolean.class)
        .block();
  }

  @Override
  public Boolean checkAdaPots(Integer epochNo) {
    return adaPotsRepository.existsByEpochNo(epochNo);
  }

  @Override
  public Boolean fetchAdaPots(List<Integer> epochNo) {
    return webClient.post()
        .uri(apiCheckAdaPotsUrl)
        .acceptCharset(StandardCharsets.UTF_8)
        .bodyValue(epochNo)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Boolean.class)
        .block();
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
      Epoch[] epoch =
          webClient.post()
              .uri(apiCheckEpochUrl)
              .acceptCharset(StandardCharsets.UTF_8)
              .bodyValue(epochNoList)
              .retrieve()
              .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
                  clientResponse -> Mono.error(
                      new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
              .bodyToMono(Epoch[].class)
              .block();
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
