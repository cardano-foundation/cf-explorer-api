package org.cardanofoundation.explorer.api.event.blocksync;

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
  private long lastBlockNo;
  private boolean hashTx;
}
