package org.cardanofoundation.explorer.api.model.response.tx;

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
  private CollateralResponse collaterals;
  private List<NoteResponse> notes;
  private List<WithdrawalResponse> withdrawals;
  private List<TxDelegationResponse> delegations;
  private List<TxMintingResponse> mints;
}