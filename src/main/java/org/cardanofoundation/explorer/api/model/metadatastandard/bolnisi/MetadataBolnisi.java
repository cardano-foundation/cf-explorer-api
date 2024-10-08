package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MetadataBolnisi {
  private String cid;
  private String tag;
  private boolean isCidVerified;
  private boolean isExternalApiAvailable;
  private List<WineryData> wineryData;
  private CertData certData;
  @JsonIgnore private boolean isOnChainMetadataValid;
}
