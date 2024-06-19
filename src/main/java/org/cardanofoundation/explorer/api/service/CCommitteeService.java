package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeMemberResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeOverviewResponse;

public interface CCommitteeService {

  CommitteeOverviewResponse getCommitteeOverview();

  BaseFilterResponse<CommitteeMemberResponse> getCommitteeMembers(Pageable pageable);

  CommitteeMemberResponse getCommitteeMemberDetail(String publicKey);
}
