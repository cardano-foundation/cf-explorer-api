package org.cardanofoundation.explorer.api.model.response;

import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DelegationResponse {
  private String txHash;
  private LocalDateTime time;
  private Long blockNo;
  private Integer epochNo;
  private Integer epochSlotNo;
  private List<String> stakeKeys;
  private List<String> pools;
}
