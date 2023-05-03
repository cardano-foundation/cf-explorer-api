package com.cardano.explorer.model.response.token;

import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

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
}
