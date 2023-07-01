package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.*;

import java.math.BigInteger;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxMetadataResponse {
  private BigInteger label;
  private String value;
}
