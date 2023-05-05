package org.cardanofoundation.explorer.api.model.response.tx;

import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TxInfoResponse {

  private String hash;
  private LocalDateTime time;
  private Integer blockNo;
  private String blockHash;
  private Integer epochSlot;
  private Integer maxEpochSlot;
  private Integer epochNo;
  private TxStatus status;
  private Integer confirmation;
  private BigInteger fee;
  private BigInteger totalOutput;

}
