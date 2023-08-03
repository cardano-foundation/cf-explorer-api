package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class PoolUpdateDetailProjectionImpl implements PoolUpdateDetailProjection {
  Long poolUpdateId;
  Long hashId;
  String poolId;
  String poolName;
  String poolView;
  String txHash;
  Timestamp time;
  BigInteger fee;
  String rewardAccount;
  String vrfKey;
  BigInteger pledge;
  Double margin;
  BigInteger cost;
  String metadataUrl;
  String metadataHash;
  BigInteger deposit;

  @Override
  public Long getPoolUpdateId() {
    return this.poolUpdateId;
  }

  @Override
  public Long getHashId() {
    return this.hashId;
  }

  @Override
  public String getPoolId() {
    return this.poolId;
  }

  @Override
  public String getPoolName() {
    return this.poolName;
  }

  @Override
  public String getPoolView() {
    return this.poolView;
  }

  @Override
  public String getTxHash() {
    return this.txHash;
  }

  @Override
  public Timestamp getTime() {
    return this.time;
  }

  @Override
  public BigInteger getFee() {
    return this.fee;
  }

  @Override
  public String getRewardAccount() {
    return this.rewardAccount;
  }

  @Override
  public String getVrfKey() {
    return this.vrfKey;
  }

  @Override
  public BigInteger getPledge() {
    return this.pledge;
  }

  @Override
  public Double getMargin() {
    return this.margin;
  }

  @Override
  public BigInteger getCost() {
    return this.cost;
  }

  @Override
  public String getMetadataUrl() {
    return this.metadataUrl;
  }

  @Override
  public String getMetadataHash() {
    return this.metadataHash;
  }

  @Override
  public BigInteger getDeposit() {
    return this.deposit;
  }
}
