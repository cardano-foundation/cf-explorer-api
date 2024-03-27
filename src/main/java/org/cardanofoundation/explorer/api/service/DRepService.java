package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;

public interface DRepService {
  BaseFilterResponse<DRepCertificateHistoryResponse> getTxDRepCertificateHistory(
      String drepHashOrDrepId, Pageable pageable);

  VotingProcedureChartResponse getVoteProcedureChart(
      String drepHashOrDrepId, GovActionType govActionType);

  BaseFilterResponse<DRepDelegatorsResponse> getDRepDelegators(
      String drepHashOrDrepId, Pageable pageable);

  DRepDetailsResponse getDRepDetails(String drepHashOrDrepId);
}
