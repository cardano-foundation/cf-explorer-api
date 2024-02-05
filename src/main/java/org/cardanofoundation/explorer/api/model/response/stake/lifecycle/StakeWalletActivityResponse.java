package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;

@Getter
@Setter
public class StakeWalletActivityResponse implements Serializable {

  private String txHash;
  private BigInteger amount;
  private BigInteger fee;
  private LocalDateTime time;
  private StakeTxType type;
  private TxStatus status;
}
