package org.cardanofoundation.explorer.api.service.impl;

import java.text.SimpleDateFormat;
import java.util.Date;

import lombok.RequiredArgsConstructor;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
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

  private final ObjectMapper objectMapper;
  private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  private static final String REDIS_PREFIX_KEY = "MARKET_DATA";
  private static final String UNDERSCORE = "_";
  private static final String LAST_UPDATED_FIELD = "last_updated";
  private static final String CURRENT_PRICE_FIELD = "current_price";
  

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

  private void publishMarketData(String currency) {
    String redisKey = String.join(UNDERSCORE, REDIS_PREFIX_KEY, currency.toUpperCase());
    Object marketDataCachedObject = redisTemplate.opsForValue().get(redisKey);
    Object marketDataObject =
        restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
    JsonNode marketDataNode = objectMapper.valueToTree(marketDataObject);
    JsonNode marketDataCachedNode = objectMapper.valueToTree(marketDataCachedObject);

    // if market data is null, do return
    if (marketDataObject == null) {
      return;
    }

    // if marketDataCachedNode is null and last_updated field is not equal, do update cache value
    if (marketDataCachedNode == null ||
        (!compareMarketDataField(marketDataNode, marketDataCachedNode, LAST_UPDATED_FIELD) &&
            !compareMarketDataField(marketDataNode, marketDataCachedNode, CURRENT_PRICE_FIELD))) {
      redisTemplate.opsForValue().set(redisKey, marketDataObject);
      // overwrite last_updated field in marketDataNode to current time
      ((ObjectNode) marketDataNode.get(0)).put(LAST_UPDATED_FIELD, dateFormat.format(new Date()));
      WebSocketMessage messageCurrentPrice = WebSocketMessage.builder()
          .payload(objectMapper.convertValue(marketDataNode, Object.class))
          .build();

      if (currency.equals("usd")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_USD);
      } else if (currency.equals("btc")) {
        messageCurrentPrice.setEventType(WebSocketEventType.CURRENT_PRICE_BTC);
      }
      applicationEventPublisher.publishEvent(new WebSocketEvent(messageCurrentPrice) {});
    }
  }

  /**
   * Compare market data field
   * @param marketDataNode
   * @param marketDataCachedNode
   * @param field
   * @return true if value of field is equal, otherwise false
   */
  private boolean compareMarketDataField(JsonNode marketDataNode, JsonNode marketDataCachedNode, String field) {
    return marketDataNode.get(0).get(field).asText().equals(marketDataCachedNode.get(0).get(field).asText());
  }

  @Scheduled(fixedDelayString = "${application.api.coin.gecko.market.delay-time}")
  public void publishMarketData() {
    publishMarketData("usd");
    publishMarketData("btc");
  }
}
