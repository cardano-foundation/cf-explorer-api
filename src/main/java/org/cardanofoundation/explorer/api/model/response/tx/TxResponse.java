package org.cardanofoundation.explorer.api.model.response.tx;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.projection.TxInstantaneousRewardsProjection;

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
  private List<TxStakeCertificate> stakeCertificates;
  private List<TxPoolCertificate> poolCertificates;
  private ProtocolParamResponse protocols;
  private ProtocolParamResponse previousProtocols;
  private List<String> genesisDelegateKeys;
  private List<TxInstantaneousRewardsProjection> instantaneousRewards;
  private String metadataHash;
  private List<TxMetadataResponse> metadata;
  private List<String> signersInformation;
}