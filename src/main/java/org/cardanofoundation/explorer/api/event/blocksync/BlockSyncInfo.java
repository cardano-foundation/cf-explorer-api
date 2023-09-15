package org.cardanofoundation.explorer.api.event.blocksync;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;

@Getter
@Setter
@Builder
public class BlockSyncInfo {
  private long blockNo;
  private String blockHash;
  private EpochSummary epochSummary;
  private boolean hashTx;
}
