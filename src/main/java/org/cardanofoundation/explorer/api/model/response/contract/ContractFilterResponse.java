package org.cardanofoundation.explorer.api.model.response.contract;

import java.io.Serializable;
import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ContractFilterResponse implements Serializable {
  private String address;
  private Long txCount;
  private BigInteger balance;
}
