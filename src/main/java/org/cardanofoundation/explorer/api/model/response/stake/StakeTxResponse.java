package org.cardanofoundation.explorer.api.model.response.stake;

import java.io.Serializable;
import java.sql.Timestamp;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;

import org.cardanofoundation.explorer.common.entity.ledgersync.LsStakeDeregistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.LsStakeRegistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistration;

@Getter
@Setter
public class StakeTxResponse implements Serializable {

  @JsonIgnore private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Long block;

  private Integer epoch;

  private Integer slotNo;

  private Integer epochSlotNo;

  @JsonIgnore private Long stakeAddressId;

  private String stakeKey;

  public StakeTxResponse() {}

  public StakeTxResponse(LsStakeRegistration stakeRegistration) {
    this.txId = stakeRegistration.getTxId();
    this.stakeAddressId = stakeRegistration.getStakeAddressId();
  }

  public StakeTxResponse(StakeRegistration stakeRegistration) {
    this.txHash = stakeRegistration.getTxHash();
    this.stakeKey = stakeRegistration.getAddress();
  }

  public StakeTxResponse(LsStakeDeregistration stakeDeRegistration) {
    this.txId = stakeDeRegistration.getTxId();
    this.stakeAddressId = stakeDeRegistration.getStakeAddressId();
  }
}
