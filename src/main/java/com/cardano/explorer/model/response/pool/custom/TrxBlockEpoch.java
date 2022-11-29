package com.cardano.explorer.model.response.pool.custom;

import java.sql.Timestamp;

public interface TrxBlockEpoch {

  Long getTxId();

  String getTxHash();

  Timestamp getTxTime();

  Long getBlockId();

  Integer getEpochNo();

  Long getSlotNo();
}
