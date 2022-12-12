package com.cardano.explorer.model.response.tx;

import com.cardano.explorer.common.enumeration.TxStatus;
import java.math.BigDecimal;
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
  private Integer epochSlot;
  private Integer maxEpochSlot;
  private Integer epochNo;
  private TxStatus status;
  private Integer confirmation;
  private BigDecimal fee;
  private BigDecimal totalOutput;

}
