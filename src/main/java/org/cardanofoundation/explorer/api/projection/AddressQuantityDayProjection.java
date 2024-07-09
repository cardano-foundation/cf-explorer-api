package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.time.Instant;

public interface AddressQuantityDayProjection {

    BigInteger getQuantity();
    Instant getDay();

    void setQuantity(BigInteger quantity);
    void setDay(Instant day);
}
