package com.cardano.explorer.model.response.address;

import com.cardano.explorer.model.response.token.TokenAddressResponse;
import java.math.BigInteger;
import java.util.List;
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
  private Boolean isContract;
}
