package com.cardano.explorer.service;

public interface MarketDataService {

  /**
   * Get market data
   * @param currency The target currency of market data (usd, btc, etc.)
   * @return data from market
   */
  Object getMarketData(String currency);
}
