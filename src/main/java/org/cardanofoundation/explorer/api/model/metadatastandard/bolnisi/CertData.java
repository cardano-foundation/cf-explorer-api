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
public class CertData {
  private List<CertDetailsData> certs;
  private boolean isPKeyVerified;
  private boolean isExternalApiAvailable;
  @JsonIgnore private String publicKey;
  @JsonIgnore private String header;
}
