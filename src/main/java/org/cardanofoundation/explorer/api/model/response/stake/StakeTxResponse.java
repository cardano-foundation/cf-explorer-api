package org.cardanofoundation.explorer.api.model.response.stake;

import java.io.Serializable;
import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeTxResponse implements Serializable {

  private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Long block;

  private Integer epoch;

  private Long slotNo;

  private Long epochSlotNo;

  private String stakeKey;

  public StakeTxResponse(TrxBlockEpochStake trxBlockEpochStake) {
    this.txId = trxBlockEpochStake.getTxId();
    this.txHash = trxBlockEpochStake.getTxHash();
    this.txTime = trxBlockEpochStake.getTxTime();
    this.block = trxBlockEpochStake.getBlockId();
    this.epoch = trxBlockEpochStake.getEpochNo();
    this.slotNo = trxBlockEpochStake.getSlotNo();
    this.epochSlotNo = trxBlockEpochStake.getEpochSlotNo();
    this.stakeKey = trxBlockEpochStake.getStakeKey();
  }

}
