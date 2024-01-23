package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LotData {
  private String signature;
  private boolean isSignatureVerified;
  private Object offChainData;
}
