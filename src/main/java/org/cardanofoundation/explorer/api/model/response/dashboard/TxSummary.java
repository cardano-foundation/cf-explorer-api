package org.cardanofoundation.explorer.api.model.response.dashboard;

import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TxSummary {

  private Long blockNo;
  private List<String> fromAddress;
  private List<String> toAddress;
  private Double amount;
  private String hash;
  private Integer epochNo;
  private Integer epochSlotNo;
  private Integer slot;
  private LocalDateTime time;
  private TxStatus status;
}
