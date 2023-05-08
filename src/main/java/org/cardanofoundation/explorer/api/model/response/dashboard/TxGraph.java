package org.cardanofoundation.explorer.api.model.response.dashboard;

import java.util.Date;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxGraph {
  private Date date;
  private int txs;
  private int simpleTxs;
  private int complexTxs;
}
