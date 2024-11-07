package org.cardanofoundation.explorer.api.service.impl;

import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;
import org.cardanofoundation.cf_explorer_aggregator.PoolAggregationRecord;
import org.cardanofoundation.cf_explorer_aggregator.PoolStatusRecord;
import org.cardanofoundation.cf_explorer_aggregator.UniqueAccountRecord;
import org.cardanofoundation.explorer.api.service.ExplorerAggregatorService;

@Service
@RequiredArgsConstructor
@Log4j2
public class ExplorerAggregatorServiceImpl implements ExplorerAggregatorService {

  private final WebClient webClient;

  @Value("${application.api.explorer-aggregator.base-url}")
  private String explorerAggregatorBaseUrl;

  @Override
  public Optional<AddressTxCountRecord> getTxCountForAddress(String address) {
    String url = explorerAggregatorBaseUrl + "/addresstxcount/" + address;
    return webClient
        .get()
        .uri(url)
        .retrieve()
        .bodyToMono(AddressTxCountRecord.class)
        .map(
            addressTxCountRecord -> {
              log.info("AddressTxCountRecord: {}", addressTxCountRecord);
              return Optional.of(addressTxCountRecord);
            })
        .onErrorReturn(Optional.empty())
        .block();
  }

  @Override
  public Optional<UniqueAccountRecord> getUniqueAccountForEpoch(int epoch) {
    String url = explorerAggregatorBaseUrl + "/uniqueaccount/" + epoch;
    return webClient
        .get()
        .uri(url)
        .retrieve()
        .bodyToMono(UniqueAccountRecord.class)
        .map(
            uniqueAccountRecord -> {
              log.info("UniqueAccountRecord: {}", uniqueAccountRecord);
              return Optional.of(uniqueAccountRecord);
            })
        .onErrorReturn(Optional.empty())
        .block();
  }

  @Override
  public Optional<PoolStatusRecord> getPoolStatusForPoolId(String poolId) {
    String url = explorerAggregatorBaseUrl + "/poolstatus/" + poolId;
    return webClient
        .get()
        .uri(url)
        .retrieve()
        .bodyToMono(PoolStatusRecord.class)
        .map(
            poolStatusRecord -> {
              log.info("PoolStatusRecord: {}", poolStatusRecord);
              return Optional.of(poolStatusRecord);
            })
        .onErrorReturn(Optional.empty())
        .block();
  }

  @Override
  public PoolAggregationRecord getLatestPoolAggregation() {
    String url = explorerAggregatorBaseUrl + "/poolstatus/latest";
    return webClient.get().uri(url).retrieve().bodyToMono(PoolAggregationRecord.class).block();
  }

  @Override
  public Optional<PoolAggregationRecord> getPoolAggregationByEpoch(int epoch) {
    String url = explorerAggregatorBaseUrl + "/poolstatus/epoch/" + epoch;
    return webClient
        .get()
        .uri(url)
        .retrieve()
        .bodyToMono(PoolAggregationRecord.class)
        .map(
            poolAggregationRecord -> {
              log.info("PoolAggregationRecord: {}", poolAggregationRecord);
              return Optional.of(poolAggregationRecord);
            })
        .onErrorReturn(Optional.empty())
        .block();
  }
}
