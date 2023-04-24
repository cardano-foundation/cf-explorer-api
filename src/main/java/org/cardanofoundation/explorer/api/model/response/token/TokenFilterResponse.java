package org.cardanofoundation.explorer.api.model.response.token;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonInclude(Include.NON_NULL)
public class TokenFilterResponse{
  private String name;
  private String displayName;
  private String policy;
  private String fingerprint;
  private Integer txCount;
  private String supply;
  private LocalDateTime createdOn;
  private TokenMetadataResponse metadata;
}
