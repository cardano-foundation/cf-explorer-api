package org.cardanofoundation.explorer.api.model.response.search;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SearchResponse {
  private Integer epoch;
  private String block;
  private String tx;
  private TokenSearchResponse token;
  private boolean validTokenName;
  private AddressSearchResponse address;
  private PoolSearchResponse pool;
  private boolean validPoolName;
  private String scriptHash;
  private Boolean isNativeScript;
}
