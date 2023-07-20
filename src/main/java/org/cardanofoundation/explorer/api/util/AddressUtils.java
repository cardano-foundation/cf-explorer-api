package org.cardanofoundation.explorer.api.util;

import com.bloxbean.cardano.client.address.util.AddressUtil;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.ledgersync.common.common.address.ShelleyAddress;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

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
      if(address.startsWith(CommonConstant.ADDRESS_PREFIX)) {
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
