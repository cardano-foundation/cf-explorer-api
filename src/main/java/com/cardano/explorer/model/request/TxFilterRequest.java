package com.cardano.explorer.model.request;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxFilterRequest {

  private Long blockNo;
  private String blockHash;
  private String address;
  private String tokenId;
  @JsonIgnore
  private Long ident;

}
