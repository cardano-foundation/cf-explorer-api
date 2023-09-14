package org.cardanofoundation.explorer.api.service;

public interface MarketDataService {

  /**
   * Get market data
   *
   * @param currency The target currency of market data (usd, btc, etc.)
   * @return data from market
   */
  Object getMarketData(String currency);

  /**
   * Publish market data to websocket
   *
   * @param currency The target currency of market data (usd, btc, etc.)
   */
  void publishMarketData(String currency);
}
