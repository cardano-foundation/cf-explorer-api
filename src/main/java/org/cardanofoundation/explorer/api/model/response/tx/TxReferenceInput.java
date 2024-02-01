package org.cardanofoundation.explorer.api.model.response.tx;

import java.math.BigInteger;
import java.util.List;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxReferenceInput {
  private String address;
  private Integer index;
  private String txHash;
  private BigInteger value;
  private List<TxMintingResponse> tokens;
  private String datumHash;
  private String datum;
  private String scriptHash;
  private String scriptType;
  private String script;
}
