package com.cardano.explorer.model.response.stake;

import java.sql.Timestamp;

public interface TrxBlockEpochStake {

  Long getTxId();

  String getTxHash();

  Timestamp getTxTime();

  Long getBlockId();

  Integer getEpochNo();

  Long getSlotNo();

  Long getEpochSlotNo();

  String getStakeKey();

}
