package com.cardano.explorer.util;

import com.bloxbean.cardano.client.address.AddressType;
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
      com.bloxbean.cardano.client.address.Address addressCheck
          = new com.bloxbean.cardano.client.address.Address(address);
      if(addressCheck.getAddressType() != AddressType.Byron) {
        ShelleyAddress shelleyAddress = new ShelleyAddress(address);
        if (shelleyAddress.containStakeAddress()) {
          //TO-DO: Move to common
          byte[] addr = shelleyAddress.getStakeReference();
          ShelleyAddress stakeShelley = new ShelleyAddress(addr);
          stakeAddress = stakeShelley.getAddress();
        }
      }
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
    }
    return stakeAddress;
  }
}
