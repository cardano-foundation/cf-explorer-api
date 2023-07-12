package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContractResponse {
  private String address;
  private String scriptHash;
  private ScriptPurposeType purpose;
  private String redeemerBytes;
  private Long redeemerMem;
  private Long redeemerSteps;
  private String datumHashIn;
  private String datumBytesIn;
  private String scriptBytes;
  private String datumHashOut;
  private String datumBytesOut;
  @JsonIgnore
  private Long txOutId;
}
