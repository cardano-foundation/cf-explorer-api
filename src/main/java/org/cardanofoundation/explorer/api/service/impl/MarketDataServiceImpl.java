package org.cardanofoundation.explorer.api.service.impl;

import java.time.Duration;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@RequiredArgsConstructor
public class MarketDataServiceImpl implements MarketDataService {

  @Value("${application.api.coin.gecko.market.base-url}")
  private String apiMarketDataUrl;

  @Value("${application.api.coin.gecko.market.interval-time}")
  private int marketTtl;

  private final RestTemplate restTemplate;

  private final ApplicationEventPublisher applicationEventPublisher;

  private final RedisTemplate<String, Object> redisTemplate;
  private static final String REDIS_PREFIX_KEY = "MARKET_DATA";
  private static final String UNDERSCORE = "_";

  public Object getMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object cachedData = redisTemplate.opsForValue().get(redisKey);
    if (cachedData == null) {
      cachedData =
          restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
      redisTemplate.opsForValue().set(redisKey, cachedData, Duration.ofSeconds(marketTtl));
    }
    return cachedData;
  }

  @Scheduled(fixedDelayString = "${application.api.coin.gecko.market.delay-time}")
  public void publishMarketData() {
    WebSocketMessage messagePriceUsd =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.CURRENT_PRICE_USD)
            .payload(getMarketData("usd"))
            .build();

    WebSocketMessage messagePriceBtc =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.CURRENT_PRICE_BTC)
            .payload(getMarketData("btc"))
            .build();

    applicationEventPublisher.publishEvent(new WebSocketEvent(messagePriceUsd) {});
    applicationEventPublisher.publishEvent(new WebSocketEvent(messagePriceBtc) {});
  }
}
