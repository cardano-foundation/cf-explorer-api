package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.concurrent.TimeUnit;

import jakarta.annotation.PostConstruct;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import reactor.core.publisher.Mono;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class MarketDataServiceImpl implements MarketDataService {

  @Value("${application.api.coin.gecko.market.base-url}")
  private String apiMarketDataUrl;

  private final WebClient webClient;

  private final RedisTemplate<String, Object> redisTemplate;

  private final ObjectMapper objectMapper;
  private static final String REDIS_PREFIX_KEY = "MARKET_DATA";
  private static final String UNDERSCORE = "_";
  private static final String LAST_UPDATED_FIELD = "last_updated";

  @PostConstruct
  void init() {
    String redisKeyBtc = String.join(UNDERSCORE, REDIS_PREFIX_KEY, "BTC");
    String redisKeyUsd = String.join(UNDERSCORE, REDIS_PREFIX_KEY, "USD");
    redisTemplate.delete(redisKeyBtc);
    redisTemplate.delete(redisKeyUsd);
  }

  public Object getMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object cachedData = redisTemplate.opsForValue().get(redisKey);
    if (cachedData == null) {
      cachedData = getWebClient(String.format(apiMarketDataUrl, currency), Object.class).block();
      JsonNode marketDataNode = objectMapper.valueToTree(cachedData);
      ((ObjectNode) marketDataNode.get(0))
          .put(
              LAST_UPDATED_FIELD,
              LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toString());
      cachedData = objectMapper.convertValue(marketDataNode, Object.class);
      redisTemplate.opsForValue().set(redisKey, cachedData);
      redisTemplate.expire(redisKey, 120000, TimeUnit.MILLISECONDS);
    }
    return cachedData;
  }

  public <T> Mono<T> getWebClient(String url, Class<T> clazz, Object... vars) {
    return webClient
        .get()
        .uri(url, vars)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(
            status -> status.is4xxClientError() || status.is5xxServerError(),
            clientResponse ->
                Mono.error(new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(clazz);
  }
}
