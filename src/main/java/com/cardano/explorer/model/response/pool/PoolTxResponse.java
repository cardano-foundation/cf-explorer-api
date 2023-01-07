package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.model.response.pool.projection.TxBlockEpochProjection;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolTxResponse implements Serializable {

  private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Integer epoch;

  private Long block;

  private Long slotNo;

  private String poolName;

  private BigDecimal pledge;

  private BigDecimal cost;

  private Double margin;

  private Long poolId;

  private Set<String> stakeKey;

  public PoolTxResponse(TxBlockEpochProjection trxBlockEpoch) {
    this.txId = trxBlockEpoch.getTxId();
    this.txHash = trxBlockEpoch.getTxHash();
    this.txTime = trxBlockEpoch.getTxTime();
    this.epoch = trxBlockEpoch.getEpochNo();
    this.slotNo = trxBlockEpoch.getSlotNo();
    this.pledge = trxBlockEpoch.getPledge();
    this.margin = trxBlockEpoch.getMargin();
    this.cost = trxBlockEpoch.getCost();
    this.poolName = trxBlockEpoch.getPoolName();
    this.poolId = trxBlockEpoch.getPoolId();
    this.block = trxBlockEpoch.getBlockNo();
  }
}
