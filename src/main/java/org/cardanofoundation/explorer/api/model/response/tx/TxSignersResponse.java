package org.cardanofoundation.explorer.api.model.response.tx;

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
public class TxSignersResponse {
  private String publicKey;
  private String delegateKey;
}
