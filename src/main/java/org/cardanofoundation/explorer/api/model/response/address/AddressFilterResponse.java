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
public class AddressFilterResponse {
  private String address;
  private Long txCount;
  private BigInteger balance;
}
