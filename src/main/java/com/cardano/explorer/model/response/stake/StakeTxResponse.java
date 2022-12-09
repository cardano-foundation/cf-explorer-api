package com.cardano.explorer.model.response.stake;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

@Getter
@Setter
public class StakeTxResponse implements Serializable {

  private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Long block;

  private Integer epoch;

  private Long slotNo;

  private String stakeKey;

  private List<String> poolNames;

  public StakeTxResponse(TrxBlockEpochStake trxBlockEpochStake) {
    this.txId = trxBlockEpochStake.getTxId();
    this.txHash = trxBlockEpochStake.getTxHash();
    this.txTime = trxBlockEpochStake.getTxTime();
    this.block = trxBlockEpochStake.getBlockId();
    this.epoch = trxBlockEpochStake.getEpochNo();
    this.slotNo = trxBlockEpochStake.getSlotNo();
    this.stakeKey = trxBlockEpochStake.getStakeKey();
  }

}
