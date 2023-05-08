package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

public interface TxGraphProjection {
  Timestamp getTime();
  Integer getTransactionNo();

  Long getMaxBlockId();
  Long getMinBlockId();
}
