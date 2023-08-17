package org.cardanofoundation.explorer.api.model.response.stake;

import java.io.Serializable;
import java.sql.Timestamp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.consumercommon.entity.StakeDeregistration;
import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration;

@Getter
@Setter
public class StakeTxResponse implements Serializable {

  @JsonIgnore
  private Long txId;

  private String txHash;

  private Timestamp txTime;

  private Long block;

  private Integer epoch;

  private Long slotNo;

  private Integer epochSlotNo;

  @JsonIgnore
  private Long stakeAddressId;

  private String stakeKey;

  public StakeTxResponse() {
  }

  public StakeTxResponse(StakeRegistration stakeRegistration) {
    this.txId = stakeRegistration.getTxId();
    this.stakeAddressId = stakeRegistration.getStakeAddressId();
  }

  public StakeTxResponse(StakeDeregistration stakeDeRegistration) {
    this.txId = stakeDeRegistration.getTxId();
    this.stakeAddressId = stakeDeRegistration.getStakeAddressId();
  }

}
