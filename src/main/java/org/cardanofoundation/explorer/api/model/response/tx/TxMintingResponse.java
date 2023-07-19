package org.cardanofoundation.explorer.api.model.response.tx;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.math.BigInteger;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
@JsonInclude(Include.NON_NULL)
public class TxMintingResponse {
  private String assetName;
  private BigInteger assetQuantity;
  private String assetId;
  private String policy;
  @JsonInclude
  private TokenMetadataResponse metadata;
  @JsonIgnore
  private Long multiAssetId;
}
