package org.cardanofoundation.explorer.api.model.response.script.smartcontract;

import lombok.*;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

import java.util.Set;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SmartContractFilterResponse {

  private String scriptHash;
  private ScriptType scriptVersion;
  private Long txCount;
  private Set<TxPurposeType> txPurposes;
}
