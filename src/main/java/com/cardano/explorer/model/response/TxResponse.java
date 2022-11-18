package com.cardano.explorer.model.response;

import com.cardano.explorer.model.response.tx.CollateralResponse;
import com.cardano.explorer.model.response.tx.ContractResponse;
import com.cardano.explorer.model.response.tx.NoteResponse;
import com.cardano.explorer.model.response.tx.SummaryResponse;
import com.cardano.explorer.model.response.tx.TxInfoResponse;
import com.cardano.explorer.model.response.tx.UTxOResponse;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TxResponse {

  private TxInfoResponse tx;
  private SummaryResponse summary;
  private UTxOResponse uTxOs;
  private List<ContractResponse> contracts;
  private List<CollateralResponse> collaterals;
  private List<NoteResponse> notes;

}