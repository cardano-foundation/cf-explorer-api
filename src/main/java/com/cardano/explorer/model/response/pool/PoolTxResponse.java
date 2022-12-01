package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.model.response.pool.custom.TrxBlockEpoch;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolTxResponse implements Serializable {

  private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Long block;

  private Integer epoch;

  private Long slotNo;

  private String poolName;

  private BigDecimal pledge;

  private BigDecimal cost;

  private Double margin;

  private String stakeKey;

  public PoolTxResponse(TrxBlockEpoch trxBlockEpoch) {
    this.txId = trxBlockEpoch.getTxId();
    this.txHash = trxBlockEpoch.getTxHash();
    this.txTime = trxBlockEpoch.getTxTime();
    this.block = trxBlockEpoch.getBlockId();
    this.epoch = trxBlockEpoch.getEpochNo();
    this.slotNo = trxBlockEpoch.getSlotNo();
  }
}
