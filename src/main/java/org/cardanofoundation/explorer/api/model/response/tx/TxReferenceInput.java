package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.*;

import java.math.BigInteger;
import java.util.List;

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
