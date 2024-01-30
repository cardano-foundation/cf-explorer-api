package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import reactor.core.publisher.Mono;

import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class MarketDataServiceImpl implements MarketDataService {

  @Value("${application.api.coin.gecko.market.base-url}")
  private String apiMarketDataUrl;

  private final WebClient webClient;

  private final ApplicationEventPublisher applicationEventPublisher;

  private final RedisTemplate<String, Object> redisTemplate;

  private final ObjectMapper objectMapper;
  private static final String REDIS_PREFIX_KEY = "MARKET_DATA";
  private static final String UNDERSCORE = "_";
  private static final String LAST_UPDATED_FIELD = "last_updated";
  private static final String CURRENT_PRICE_FIELD = "current_price";

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
    }
    return cachedData;
  }

  private void publishMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object marketDataCachedObject = redisTemplate.opsForValue().get(redisKey);
    Object marketDataObject =
        getWebClient(String.format(apiMarketDataUrl, currency), Object.class).block();
    JsonNode marketDataNode = objectMapper.valueToTree(marketDataObject);
    JsonNode marketDataCachedNode = objectMapper.valueToTree(marketDataCachedObject);

    // if market data is null, do return
    if (marketDataObject == null) {
      return;
    }

    // if marketDataCachedNode is null and current_price field is not equal, do update cache value
    if (marketDataCachedNode.isNull()
        || !compareMarketDataField(marketDataNode, marketDataCachedNode, CURRENT_PRICE_FIELD)) {
      // overwrite last_updated field in marketDataNode to current time
      ((ObjectNode) marketDataNode.get(0))
          .put(
              LAST_UPDATED_FIELD,
              LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toString());
      marketDataObject = objectMapper.convertValue(marketDataNode, Object.class);
      WebSocketMessage messageCurrentPrice =
          WebSocketMessage.builder().payload(marketDataObject).build();

      if (currency.equals("usd")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_USD);
      } else if (currency.equals("btc")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_BTC);
      }
      redisTemplate.opsForValue().set(redisKey, marketDataObject);
      applicationEventPublisher.publishEvent(new WebSocketEvent(messageCurrentPrice) {});
    }
  }

  /**
   * Compare market data field
   *
   * @param marketDataNode
   * @param marketDataCachedNode
   * @param field
   * @return true if value of field is equal, otherwise false
   */
  private boolean compareMarketDataField(
      JsonNode marketDataNode, JsonNode marketDataCachedNode, String field) {
    return marketDataNode
        .get(0)
        .get(field)
        .asText()
        .equals(marketDataCachedNode.get(0).get(field).asText());
  }

  @Scheduled(fixedDelayString = "${application.api.coin.gecko.market.delay-time}")
  public void publishMarketData() {
    publishMarketData("usd");
    publishMarketData("btc");
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
