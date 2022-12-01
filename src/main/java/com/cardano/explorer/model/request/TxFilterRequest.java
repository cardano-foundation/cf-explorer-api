package com.cardano.explorer.model.request;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxFilterRequest {

  private Integer blockNo;
  private String address;

}
