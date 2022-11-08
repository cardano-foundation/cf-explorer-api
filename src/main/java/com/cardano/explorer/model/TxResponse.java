package com.cardano.explorer.model;

import com.cardano.explorer.common.enumeration.TxStatus;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxResponse {

  private String hash;

  private LocalDateTime time;

  private Integer blockNo;

  private Integer epochNo;

  private TxStatus status;

  private Integer confirmation;

  private BigDecimal fee;

  private BigDecimal totalOutput;

}
