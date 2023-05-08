package org.cardanofoundation.explorer.api.model.response;

import java.math.BigInteger;
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

  private BigInteger totalFees;

  private BigInteger totalOutput;

  private String slotLeader;

  private Integer confirmation;

}
