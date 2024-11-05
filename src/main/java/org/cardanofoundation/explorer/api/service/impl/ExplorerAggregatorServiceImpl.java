package org.cardanofoundation.explorer.api.service.impl;

import java.util.List;
import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;
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

}
