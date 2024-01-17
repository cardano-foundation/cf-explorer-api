package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.service.WebClientService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class WebClientServiceImpl implements WebClientService {

  private final WebClient webClient;

  @Value("${application.api.coin.gecko.market.base-url}")
  private String apiMarketDataUrl;
  @Override
  public <T> Mono<T> callWebClient(String url, Class<T> clazz, Object... vars) {
    return webClient.get()
        .uri(url, vars)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(clazz);
  }
  @Override
  public  <T> Mono<T> postWebClient(String url, Class<T> clazz, Object... vars) {
    return webClient.post()
        .uri(url, vars)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse -> Mono.error(
                new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(clazz);
  }
}
