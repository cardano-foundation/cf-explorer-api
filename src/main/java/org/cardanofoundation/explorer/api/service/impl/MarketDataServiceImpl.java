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

  private final RestTemplate restTemplate;

  private final ApplicationEventPublisher applicationEventPublisher;

  private final RedisTemplate<String, Object> redisTemplate;

  public Object getMarketData(String currency) {
    Object cachedData = redisTemplate.opsForValue().get(currency);
    if (cachedData == null) {
      cachedData =
          restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
      redisTemplate.opsForValue().set(currency, cachedData, Duration.ofSeconds(30));
    }
    return cachedData;
  }

  @Scheduled(fixedDelay = 4000)
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
