package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import java.util.List;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class MetadataBolnisi {
  private String cid;
  private boolean isCidVerified;
  private List<WineryData> wineryData;
}
