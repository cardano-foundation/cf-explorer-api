package org.cardanofoundation.explorer.api.model.response.address;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.api.projection.AddressResponseProjection;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AddressResponse implements AddressResponseProjection {
  private String address;
  private Long txCount;
  private BigInteger balance;
  private String stakeAddress;
  private String scriptHash;
  private boolean isAssociatedSmartContract;
  private boolean isAssociatedNativeScript;

  public AddressResponse(String address, BigInteger balance) {
    this.address = address;
    this.balance = balance;
  }

  public static AddressResponse fromProjection(AddressResponseProjection projection) {
    if (projection == null) {
      return null;
    }

    return AddressResponse.builder()
        .address(projection.getAddress())
        .txCount(projection.getTxCount())
        .balance(projection.getBalance())
        .build();
  }
}
