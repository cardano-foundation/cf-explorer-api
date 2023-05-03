package org.cardanofoundation.explorer.api.model.response.token;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PolicyResponse {
  private String policyId;
  private Integer totalToken;
  private String policyScript;
}
