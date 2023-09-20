package org.cardanofoundation.explorer.api.model.response.protocol;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class FixedProtocol {

  Double activeSlotsCoeff;
  Object genDelegs;
  Integer updateQuorum;
  String networkId;
  Object initialFunds;
  BigInteger maxLovelaceSupply;
  Integer networkMagic;
  Integer epochLength;
  String timestamp;
  Integer slotsPerKESPeriod;
  Integer slotLength;
  Integer maxKESEvolutions;
  Integer securityParam;
}
