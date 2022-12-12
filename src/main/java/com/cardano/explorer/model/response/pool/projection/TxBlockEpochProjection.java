package com.cardano.explorer.model.response.pool.projection;

import java.sql.Timestamp;

public interface TxBlockEpochProjection {

  Long getTxId();

  String getTxHash();

  Timestamp getTxTime();

  Long getBlockId();

  Integer getEpochNo();

  Long getSlotNo();
}
