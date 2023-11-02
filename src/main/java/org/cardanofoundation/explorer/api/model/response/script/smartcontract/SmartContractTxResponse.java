package org.cardanofoundation.explorer.api.model.response.script.smartcontract;

import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SmartContractTxResponse {

  @JsonIgnore
  private Long txId;
  private String hash;
  private Timestamp time;
  private Long blockNo;
  private Integer epochNo;
  private Integer epochSlotNo;
  private Integer absoluteSlot;
  private Set<String> addresses;
  private List<ScriptPurposeType> scriptPurposeTypes;
}
