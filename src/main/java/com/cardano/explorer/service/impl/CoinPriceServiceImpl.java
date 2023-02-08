package com.cardano.explorer.service.impl;

import com.cardano.explorer.service.CoinPriceService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class CoinPriceServiceImpl implements CoinPriceService {

  @Value("${application.api.coin.gecko.simple.price.base-url}")
  private String apiSimpleCoinUrl;

  private final RestTemplate restTemplate;

  public CoinPriceServiceImpl(RestTemplate restTemplate) {
    this.restTemplate = restTemplate;
  }

  @Cacheable("coin-price")
  public Object getCoinPrice() {
    return restTemplate.getForObject(apiSimpleCoinUrl, Object.class);
  }
}