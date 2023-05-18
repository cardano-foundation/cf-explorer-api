package org.cardanofoundation.explorer.api.model.response.protocol;

import java.math.BigInteger;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class FixedProtocol {

  Float activeSlotsCoeff;
  Object genDelegs;
  Integer updateQuorum;
  String networkId;
  String initialFunds;
  BigInteger maxLovelaceSupply;
  Integer networkMagic;
  Integer epochLength;
  String systemStart;
  Integer slotsPerKESPeriod;
  Integer slotLength;
  Integer maxKESEvolutions;
  Integer securityParam;
}
