package org.cardanofoundation.explorer.api.model.response.token;

import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TokenMintTxResponse {
  private String txHash;
  private String amount;
  private LocalDateTime time;

}
