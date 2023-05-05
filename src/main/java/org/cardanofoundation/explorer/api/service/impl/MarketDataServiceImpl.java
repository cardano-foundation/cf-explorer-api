package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.service.MarketDataService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@RequiredArgsConstructor
public class MarketDataServiceImpl implements MarketDataService {

  @Value("${application.api.coin.gecko.market.base-url}")
  private String apiMarketDataUrl;

  private final RestTemplate restTemplate;

  @Cacheable(value = "market", key = "#currency", cacheManager = "cacheManager")
  public Object getMarketData(String currency) {
    return restTemplate.getForObject(String.format(apiMarketDataUrl, currency), Object.class);
  }
}