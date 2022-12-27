package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.model.response.contract.ContractFilterResponse;
import java.math.BigDecimal;
import java.util.List;
import org.springframework.data.domain.Pageable;

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
   * Get the highest and lowest balance by address
   *
   * @param address wallet address
   * @return the highest and lowest balance
   */
  List<BigDecimal> getAddressMinMaxBalance(String address);

  /**
   * Get list contract
   *
   * @param pageable page information
   * @return list contract information in this page
   */
  BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable);
}
