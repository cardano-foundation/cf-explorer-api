package com.cardano.explorer.model.response.tx;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class TxPolicyResponse {
  private String policyId;
  private Integer totalToken;
  private String policyScript;
}
