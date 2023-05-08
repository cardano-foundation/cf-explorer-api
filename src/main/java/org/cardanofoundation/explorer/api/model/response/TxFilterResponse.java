package org.cardanofoundation.explorer.api.model.response;

import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxFilterResponse implements Serializable {

  private String hash;
  private Long blockNo;
  private String blockHash;
  private Integer epochNo;
  private Integer epochSlotNo;
  private Integer slot;
  private LocalDateTime time;
  private List<String> addressesInput;
  private List<String> addressesOutput;
  private BigInteger fee;
  private BigInteger totalOutput;

}
