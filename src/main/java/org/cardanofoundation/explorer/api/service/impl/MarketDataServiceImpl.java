package org.cardanofoundation.explorer.api.service.impl;

import java.util.ArrayList;
import java.util.LinkedHashMap;
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
  private static final String REDIS_PREFIX_KEY = "MARKET_DATA";
  private static final String UNDERSCORE = "_";

  public Object getMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object cachedData = redisTemplate.opsForValue().get(redisKey);
    if (cachedData == null) {
      cachedData =
          restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
      redisTemplate.opsForValue().set(redisKey, cachedData);
    }
    return cachedData;
  }

  public void publishMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object marketDataCachedObject = redisTemplate.opsForValue().get(redisKey);
    Object marketDataObject =
        restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
    LinkedHashMap<String, Object> marketData =
        (LinkedHashMap<String, Object>) ((ArrayList) marketDataObject).get(0);
    LinkedHashMap<String, Object> marketDataCached =
        (LinkedHashMap<String, Object>) ((ArrayList) marketDataCachedObject).get(0);
    if (!marketData
        .get("last_updated")
        .toString()
        .equals(marketDataCached.get("last_updated").toString())) {
      WebSocketMessage messageCurrentPrice = WebSocketMessage.builder().payload(marketData).build();
      if (currency.equals("usd")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_USD);
      } else if (currency.equals("btc")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_BTC);
      }
      redisTemplate.opsForValue().set(redisKey, marketDataObject);
      applicationEventPublisher.publishEvent(new WebSocketEvent(messageCurrentPrice) {});
    }
  }

  @Scheduled(fixedDelayString = "${application.api.coin.gecko.market.delay-time}")
  public void publishMarketData() {
    publishMarketData("usd");
    publishMarketData("btc");
  }
}
