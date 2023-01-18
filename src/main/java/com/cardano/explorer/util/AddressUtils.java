package com.cardano.explorer.util;

import com.bloxbean.cardano.client.address.util.AddressUtil;
import com.cardano.explorer.exception.BusinessCode;
import com.sotatek.cardano.ledgersync.common.address.ShelleyAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;

public class AddressUtils {

  private AddressUtils() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Check address
   * @param address address
   * @return stake address if exits, throw exception if not valid
   */
  public static String checkStakeAddress(String address) {
    String stakeAddress = null;
    try {
      if(address.startsWith("addr")) {
        ShelleyAddress shelleyAddress = new ShelleyAddress(address);
        if (shelleyAddress.containStakeAddress()) {
          //TO-DO: Move to common
          byte[] addr = shelleyAddress.getStakeReference();
          ShelleyAddress stakeShelley = new ShelleyAddress(addr);
          stakeAddress = stakeShelley.getAddress();
        }
      } else if(!AddressUtil.isValidAddress(address)) {
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
    }
    return stakeAddress;
  }
}
