package org.cardanofoundation.explorer.api.model.response;

import java.time.LocalDateTime;
import java.util.List;

import lombok.*;

import org.cardanofoundation.explorer.api.model.response.address.DelegationPoolResponse;

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
  private Integer slotNo;
  private List<String> stakeKeys;
  private List<DelegationPoolResponse> pools;
}
