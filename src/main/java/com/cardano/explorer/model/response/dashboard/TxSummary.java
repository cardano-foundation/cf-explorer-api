package com.cardano.explorer.model.response.dashboard;

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
}
