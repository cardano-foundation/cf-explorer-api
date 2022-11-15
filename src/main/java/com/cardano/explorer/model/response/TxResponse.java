package com.cardano.explorer.model.response;

import com.cardano.explorer.common.enumeration.TxStatus;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxResponse {

  private String hash;

  private LocalDateTime time;

  private Integer blockNo;

  private Integer epochNo;

  private TxStatus status;

  private Integer confirmation;

  private BigDecimal fee;

  private BigDecimal totalOutput;

  private List<TxOutResponse> utxOInputList;

  private List<TxOutResponse> utxOOutputList;

  private List<TxOutResponse> stakeAddressTxInputList;

  private List<TxOutResponse> stakeAddressTxOutputList;

}