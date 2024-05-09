package org.cardanofoundation.explorer.api.model.response.address;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AddressResponse {
  private String address;
  private Long txCount;
  private BigInteger balance;
  private String stakeAddress;
  private String scriptHash;
  private boolean isAssociatedSmartContract;
  private boolean isAssociatedNativeScript;

  public AddressResponse(String address, Long txCount, BigInteger balance) {
    this.address = address;
    this.txCount = txCount;
    this.balance = balance;
  }

  public AddressResponse(String address, BigInteger balance) {
    this.address = address;
    this.balance = balance;
  }
}
