package org.cardanofoundation.explorer.api.model.response;

import java.math.BigInteger;

import lombok.*;

@Getter
@Setter
@Builder
public class PotsOverviewResponse {
  BigInteger depositsAndFees;
  BigInteger rewards;
  BigInteger treasury;
  BigInteger reserves;
  Integer epoch;
}
