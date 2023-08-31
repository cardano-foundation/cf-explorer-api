package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import org.cardanofoundation.explorer.api.service.impl.MarketDataServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
public class MarketDataServiceTest {

  @Mock private RestTemplate restTemplate;

  @InjectMocks private MarketDataServiceImpl marketDataService;
  @Mock RedisTemplate<String, Object> redisTemplate;
  @Mock ValueOperations<String, Object> valueOperations;

  @Test
  void testGetMarketData_thenReturn() {
    String currency = "usd";
    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    when(valueOperations.get(currency)).thenReturn(null);
    ReflectionTestUtils.setField(marketDataService, "apiMarketDataUrl", "localhost:8080");
    when(restTemplate.getForObject(String.format("localhost:8080", currency), Object.class))
        .thenReturn(123);

    var response = marketDataService.getMarketData(currency);
    Assertions.assertEquals(response, 123);
  }
}
