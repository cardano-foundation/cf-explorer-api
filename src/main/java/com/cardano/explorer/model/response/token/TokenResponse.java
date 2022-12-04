package com.cardano.explorer.model.response.token;

import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TokenResponse {
  private String name;
  private String policy;
  private String fingerprint;
  private Integer txCount;
  private String supply;
  private LocalDateTime createdOn;
}
