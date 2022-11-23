package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface TxGraphProjection {
  Timestamp getTime();
  Integer getTransactionNo();
}
