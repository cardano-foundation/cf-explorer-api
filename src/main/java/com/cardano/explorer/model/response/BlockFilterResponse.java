package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BlockFilterResponse implements Serializable {

  private Integer blockNo;

  private String hash;

  private LocalDateTime time;

  private Long txCount;

  private BigDecimal totalFees;

  private BigDecimal totalOutput;

  private String slotLeader;

}
