package org.cardanofoundation.explorer.api.model.response.token;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.math.BigInteger;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(Include.NON_NULL)
public class TokenAddressResponse {
  private Long addressId;
  private Long numberOfPaymentAddress;
  private String address;
  private String stakeAddress;
  private String policy;
  private TokenMetadataResponse metadata;
  private String name;
  private String displayName;
  private String fingerprint;
  private BigInteger quantity;
}
