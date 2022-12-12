package com.cardano.explorer.model.response.tx;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
@JsonInclude(Include.NON_NULL)
public class TxMintingResponse {
  private String assetName;
  private BigDecimal assetQuantity;
  private String assetId;
  private TxPolicyResponse policy;
}
