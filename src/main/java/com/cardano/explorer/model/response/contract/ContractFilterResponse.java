package com.cardano.explorer.model.response.contract;

import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ContractFilterResponse implements Serializable {
  private String address;
  private Long txCount;
  private BigDecimal balance;
}
