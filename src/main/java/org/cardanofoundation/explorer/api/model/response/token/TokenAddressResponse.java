package org.cardanofoundation.explorer.api.model.response.token;

import java.math.BigInteger;

import lombok.*;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import org.cardanofoundation.explorer.api.common.enumeration.AddressType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(Include.NON_NULL)
public class TokenAddressResponse {
  private Long addressId;
  private String address;
  private AddressType addressType;
  private String policy;
  private TokenMetadataResponse metadata;
  private String name;
  private String displayName;
  private String fingerprint;
  private BigInteger quantity;
}
