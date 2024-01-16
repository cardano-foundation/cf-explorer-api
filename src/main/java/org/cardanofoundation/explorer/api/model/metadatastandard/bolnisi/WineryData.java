package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;


import java.util.List;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Getter
@Setter
@Builder
public class WineryData {
  private String wineryId;
  private boolean isPKeyVerified;
  private List<LotData> lots;
  @JsonIgnore
  private String publicKey;
  @JsonIgnore
  private String header;
}
