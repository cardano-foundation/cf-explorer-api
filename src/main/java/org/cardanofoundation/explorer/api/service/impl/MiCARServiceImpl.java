package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import reactor.core.publisher.Mono;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.micar.AddressCarbonEmissionResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.service.ExplorerAggregatorService;
import org.cardanofoundation.explorer.api.service.MiCARService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
public class MiCARServiceImpl implements MiCARService {

  private final ExplorerAggregatorService explorerAggregatorService;
  private final StakeAddressRepository stakeAddressRepository;
  private final WebClient webClient;

  @Value("${application.api.micar.overview}")
  private String apiMicarOverviewUrl;

  @Value("${application.api.micar.historical}")
  private String apiMicarHistoricalUrl;

  @Value("${application.api.micar.public-key}")
  private String micarPublicKey;

  private final AddressRepository addressRepository;

  @Override
  public AddressCarbonEmissionResponse getCarbonEmissionsByAddressAndPool(String address) {
    if (Objects.isNull(address)) {
      return AddressCarbonEmissionResponse.builder().build();
    }
    if (address.startsWith(CommonConstant.PREFIXED_STAKE_KEY)) {
      Optional<StakeAddress> stakeAddress = stakeAddressRepository.findByView(address);
      if (stakeAddress.isEmpty()) {
        return AddressCarbonEmissionResponse.builder().build();
      }
      Long txCount = explorerAggregatorService
              .getTxCountForAddress(stakeAddress.get().getView()).map(AddressTxCountRecord::getTxCount)
              .orElse(0L);

      return AddressCarbonEmissionResponse.builder()
          .stakeAddress(address)
          .txCount(txCount)
          .carbonEmissionPerTx(CommonConstant.MiCAR.CO2_EMISSION_PER_TX)
          .build();
    } else {
      try {
        AddressUtils.checkStakeAddress(address);
        Optional<Address> addr = addressRepository.findFirstByAddress(address);
        if (addr.isEmpty()) {
          return AddressCarbonEmissionResponse.builder().build();
        }
        Long txCount = explorerAggregatorService.getTxCountForAddress(address)
                .map(AddressTxCountRecord::getTxCount)
                .orElse(0L);
        
        return AddressCarbonEmissionResponse.builder()
            .address(address)
            .txCount(txCount)
            .carbonEmissionPerTx(CommonConstant.MiCAR.CO2_EMISSION_PER_TX)
            .build();
      } catch (Exception e) {
        return AddressCarbonEmissionResponse.builder().build();
      }
    }
  }

  @SingletonCall(typeToken = TypeTokenGson.MICAR, expireAfterSeconds = 200)
  public Object getCarbonEmissionsOverview() {
    return webClient
        .get()
        .uri(apiMicarOverviewUrl, micarPublicKey)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(
            status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse ->
                Mono.error(new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Object.class)
        .block();
  }

  @SingletonCall(typeToken = TypeTokenGson.MICAR, expireAfterSeconds = 200)
  public Object getCarbonEmissionsHistorical() {
    return webClient
        .get()
        .uri(apiMicarHistoricalUrl, micarPublicKey)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(
            status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse ->
                Mono.error(new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(Object.class)
        .block();
  }
}
