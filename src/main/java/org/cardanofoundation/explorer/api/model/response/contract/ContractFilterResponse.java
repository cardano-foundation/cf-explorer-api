package org.cardanofoundation.explorer.api.model.response.contract;

import java.io.Serializable;
import java.math.BigInteger;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContractFilterResponse implements Serializable {
  private String address;
  private Long txCount;
  private BigInteger balance;
}
