package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.time.LocalDate;

public interface AggregateAddressBalanceProjection {

  LocalDate getDay();
  BigInteger getBalance();
  Long getAddressId();
}
