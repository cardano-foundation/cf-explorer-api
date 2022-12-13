package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import java.util.List;

public interface AddressService {

  /**
   * Get detail wallet address info
   *
   * @param address wallet address
   * @return wallet address info
   */
  AddressResponse getAddressDetail(String address);

  /**
   * Get address analytics
   * @param type type of analytics (day, week, month, 3month)
   * @return
   */
  List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type);
}
