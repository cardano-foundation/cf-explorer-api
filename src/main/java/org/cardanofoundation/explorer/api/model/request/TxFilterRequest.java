package org.cardanofoundation.explorer.api.model.request;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Getter
@Setter
public class TxFilterRequest {

  private Long blockNo;
  private String blockHash;
  private String address;
  private String tokenId;
  @JsonIgnore private Long ident;
}
