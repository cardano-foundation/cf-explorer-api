package org.cardanofoundation.explorer.api.model.response;

import java.io.Serializable;
import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BlockFilterResponse implements Serializable {

  private Integer blockNo;

  private Long slotNo;

  private Integer epochNo;

  private Integer epochSlotNo;

  private String hash;

  private LocalDateTime time;

  private String slotLeader;
}
