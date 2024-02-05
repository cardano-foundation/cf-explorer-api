package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LotData {
  private String signature;
  private boolean isSignatureVerified;
  private Object offChainData;
}
