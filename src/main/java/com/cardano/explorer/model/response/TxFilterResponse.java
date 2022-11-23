package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxFilterResponse implements Serializable {

  private String hash;
  private Integer blockNo;
  private Integer epochNo;
  private Integer epochSlotNo;
  private Integer slot;
  private List<String> addressesInput;
  private List<String> addressesOutput;
  private BigDecimal fee;
  private BigDecimal totalOutput;

}
