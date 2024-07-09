package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.time.LocalDate;

public interface AddressQuantityDayProjection {

    BigInteger getQuantity();
    LocalDate getDay();

    void setQuantity(BigInteger quantity);
    void setDay(LocalDate day);
}
