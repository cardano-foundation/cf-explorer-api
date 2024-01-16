package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Getter
@Setter
@Builder
public class LotData {
  @JsonIgnore
  private String signature;
  private boolean isSignatureVerified;
  private Object offChainData;
}
