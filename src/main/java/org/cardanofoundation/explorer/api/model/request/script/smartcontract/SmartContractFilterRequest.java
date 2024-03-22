package org.cardanofoundation.explorer.api.model.request.script.smartcontract;

import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SmartContractFilterRequest {

  ScriptType scriptVersion;
  Set<TxPurposeType> txPurpose;

  @JsonIgnore private Boolean isScriptReward;
  @JsonIgnore private Boolean isScriptCert;
  @JsonIgnore private Boolean isScriptSpend;
  @JsonIgnore private Boolean isScriptMint;
  @JsonIgnore private Boolean isScriptVote;
  @JsonIgnore private Boolean isScriptPropose;
  @JsonIgnore private Boolean isScriptAny;
  @JsonIgnore private Boolean isScriptNone;
}
