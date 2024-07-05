package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface AddressQuantityProjection {

    String getAddress();
    BigInteger getQuantity();
    void setAddress(String address);
    void setQuantity(BigInteger quantity);
}
