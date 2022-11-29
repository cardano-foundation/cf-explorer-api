package com.cardano.explorer.service;

import com.cardano.explorer.model.response.address.StakeAddressResponse;

public interface StakeAddressService {
  /**
   * Get detail stake address info
   *
   * @param address stake address
   * @return stake address info
   */
  StakeAddressResponse getStakeAddress(String address);
}
