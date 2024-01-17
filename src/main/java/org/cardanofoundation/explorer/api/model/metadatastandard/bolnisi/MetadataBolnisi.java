package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MetadataBolnisi {
  private String cid;
  private boolean isCidVerified;
  private boolean isExternalApiAvailable;
  private List<WineryData> wineryData;
}
