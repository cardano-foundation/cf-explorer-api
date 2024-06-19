package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeMemberResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeOverviewResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

public interface CCommitteeService {

  CommitteeOverviewResponse getCommitteeOverview();

  BaseFilterResponse<CommitteeMemberResponse> getCommitteeMembers(Pageable pageable);

  CommitteeMemberResponse getCommitteeMemberDetail(String publicKey);

  VotingProcedureChartResponse getVoteProcedureChart(String publicKey, GovActionType govActionType);
}
