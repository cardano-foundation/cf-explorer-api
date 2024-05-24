package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.drep.DRepFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepOverviewResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

public interface DRepService {
  BaseFilterResponse<DRepCertificateHistoryResponse> getTxDRepCertificateHistory(
      String drepHashOrDrepId, Pageable pageable);

  VotingProcedureChartResponse getVoteProcedureChart(
      String dRepHashOrId, GovActionType govActionType);

  BaseFilterResponse<DRepDelegatorsResponse> getDRepDelegators(
      String drepHashOrDrepId, Pageable pageable);

  DRepDetailsResponse getDRepDetails(String drepHashOrDrepId);

  DRepOverviewResponse getDRepOverview();

  BaseFilterResponse<DRepFilterResponse> getDRepsByFilter(
      DRepFilterRequest dRepFilterRequest, Pageable pageable);

  DRepRangeValuesResponse getDRepRangeValues();
}
