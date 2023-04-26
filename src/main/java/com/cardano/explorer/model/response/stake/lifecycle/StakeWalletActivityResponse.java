package com.cardano.explorer.model.response.stake.lifecycle;

import com.cardano.explorer.common.enumeration.StakeTxType;
import com.cardano.explorer.common.enumeration.TxStatus;
import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeWalletActivityResponse implements Serializable {

  private String txHash;
  private BigInteger amount;
  private LocalDateTime time;
  private StakeTxType type;
  private TxStatus status;
}
