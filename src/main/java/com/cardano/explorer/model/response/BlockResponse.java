package com.cardano.explorer.model.response;

import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BlockResponse {

  private String hash;

  private Integer epochNo;

  private Integer blockNo;

  private Long slotNo;

  private Integer epochSlotNo;

  private Integer size;

  private Long txCount;

  private LocalDateTime time;

  private Integer previousBlock;

  private Integer nextBlock;

}
