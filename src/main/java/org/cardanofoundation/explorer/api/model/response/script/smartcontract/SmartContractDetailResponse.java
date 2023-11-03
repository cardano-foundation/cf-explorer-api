package org.cardanofoundation.explorer.api.model.response.script.smartcontract;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SmartContractDetailResponse {

  private String scriptHash;
  private ScriptType scriptType;
  private List<String> associatedAddresses;
}
