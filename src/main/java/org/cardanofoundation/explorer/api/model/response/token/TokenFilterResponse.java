package org.cardanofoundation.explorer.api.model.response.token;

import java.time.LocalDateTime;

import lombok.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(Include.NON_NULL)
public class TokenFilterResponse {
  private Long id;
  private String name;
  private String displayName;
  private String policy;
  private boolean policyIsNativeScript;
  private String fingerprint;
  private Integer txCount;
  private String supply;
  private String volumeIn24h;
  private String totalVolume;
  private Long numberOfHolders;
  private LocalDateTime createdOn;
  private TokenMetadataResponse metadata;
  @JsonIgnore private String unit;
}
