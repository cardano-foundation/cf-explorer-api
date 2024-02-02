package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import reactor.core.publisher.Mono;

import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.service.NewsService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

@Service
@RequiredArgsConstructor
public class NewsServiceImpl implements NewsService {

  @Value("${application.api.news.base-url}")
  private String apiNewsDataUrl;

  private final WebClient webClient;

  @SingletonCall(typeToken = TypeTokenGson.NEWS, expireAfterSeconds = 200)
  public Object getNews(Integer limit, Integer offset) {
    return webClient
        .get()
        .uri(String.format(apiNewsDataUrl, limit, offset))
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
