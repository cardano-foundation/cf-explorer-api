package org.cardanofoundation.explorer.api.model.redis;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BlockSyncMessage implements Serializable {
  private String lastBlockHash;
  private Long lastBlockNo;
  private boolean hasTx;
}
