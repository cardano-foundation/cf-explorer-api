package org.cardanofoundation.explorer.api.model.response.tx;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

import org.cardanofoundation.explorer.api.common.enumeration.RedeemerCertType;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptPurposeType;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContractResponse {
  @JsonIgnore private String address;
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
  @JsonIgnore private Long txOutId;
  @JsonIgnore private String stakeAddress;
  private List<TokenAddressResponse> mintingTokens;
  private List<TokenAddressResponse> burningTokens;
  private Integer utxoIndex;
  private String utxoHash;
  private RedeemerCertType redeemerCertType;
  private List<TxReferenceInput> referenceInputs;
  private List<String> executionInputs;
  private List<String> executionOutputs;
}
