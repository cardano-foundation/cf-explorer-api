package org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CertDetailsData {
  private String signature;
  private Object offChainData;
  private boolean isSignatureVerified;
  @JsonIgnore private String certNo;
}
