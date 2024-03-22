package org.cardanofoundation.explorer.api.model.response.drep;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DRepDelegatorDetails {

  String stakeAddress;

  String txHash;

  BigInteger blockTime;

  Integer fee;
}
