package com.cardano.explorer.model.response;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BlockResponse {

  private String hash;

  private LocalDateTime time;

  private Long txCount;

  private Integer epochNo;

  private Integer blockNo;

  private Long slotNo;

  private Integer epochSlotNo;

  private BigDecimal totalFees;

  private BigDecimal totalOutput;

  private String slotLeader;

}
