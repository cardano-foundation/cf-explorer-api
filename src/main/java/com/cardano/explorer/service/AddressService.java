package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import java.math.BigDecimal;
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
   *
   * @param address wallet address
   * @param type type of analytics (day, week, month, 3month)
   * @return list value balance
   */
  List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type);

  /**
   * Get the highest balance by address
   *
   * @param address wallet address
   * @return the highest balance
   */
  BigDecimal getAddressMaxBalance(String address);

  /**
   * Get the lowest balance by address
   *
   * @param address wallet address
   * @return the lowest balance
   */
  BigDecimal getAddressMinBalance(String address);
}
