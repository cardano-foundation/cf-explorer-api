package org.cardanofoundation.explorer.api.model.response.token;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

@Getter
@Setter
@Builder
public class PolicyResponse {
  private String policyId;
  private Integer totalToken;
  private String policyScript;
  private boolean isNativeScript = false;
  private boolean isSmartContract = false;
}
