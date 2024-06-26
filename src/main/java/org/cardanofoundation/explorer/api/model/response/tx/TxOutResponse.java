package org.cardanofoundation.explorer.api.model.response.tx;

import java.math.BigInteger;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(Include.NON_NULL)
@EqualsAndHashCode
public class TxOutResponse {
  private String address;
  private String index;
  private String txHash;
  private BigInteger value;
  @EqualsAndHashCode.Exclude private String assetId;
  private List<TxMintingResponse> tokens;
  private String stakeAddress;
}
