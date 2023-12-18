package org.cardanofoundation.explorer.api.model.response.token;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip25.MetadataCIP25;

@Getter
@Setter
public class TokenResponse {

  private String name;
  private String displayName;
  private String policy;
  private String fingerprint;
  private Integer txCount;
  private String supply;
  private String volumeIn24h;
  private String totalVolume;
  private Long numberOfHolders;
  private LocalDateTime createdOn;
  private TokenMetadataResponse metadata;
  private Timestamp tokenLastActivity;
  private TokenType tokenType;
  private String metadataJson;
  private MetadataCIP25 metadataCIP25;
}
