package org.cardanofoundation.explorer.api.model.response.script.smartcontract;

import lombok.*;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SmartContractFilterResponse {
  private String scriptHash;
  private ScriptType version;
  private List<String> associatedAddress;
}
