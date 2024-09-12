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
public class CertData {
  private String signature;
  private Object offChainData;
  private boolean isSignatureVerified;
  private boolean isPKeyVerified;
  private boolean isExternalApiAvailable;
  @JsonIgnore private String certNo;
  @JsonIgnore private String publicKey;
  @JsonIgnore private String header;
}
