package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressFilterResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.model.response.contract.ContractFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import java.math.BigInteger;
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
  List<BigInteger> getAddressMinMaxBalance(String address);

  /**
   * Get list contract
   *
   * @param pageable page information
   * @return list contract information in this page
   */
  BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable);

  /**
   * Get top address
   *
   * @param pageable page information
   * @return return list address sort by balance
   */
  BaseFilterResponse<AddressFilterResponse> getTopAddress(Pageable pageable);


  /**
   * Get list token by display name
   *
   * @param pageable page information
   * @param address wallet address
   * @param displayName display name of token
   * @return list token by display name
   */
  BaseFilterResponse<TokenAddressResponse> getTokenByDisplayName(Pageable pageable, String address, String displayName);
}
