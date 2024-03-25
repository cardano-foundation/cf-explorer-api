package org.cardanofoundation.explorer.api.service.impl;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.common.enumeration.VoteType;
import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;

@Service
@RequiredArgsConstructor
public class GovernanceActionServiceImpl implements GovernanceActionService {

  private final DRepRegistrationRepository dRepRegistrationRepository;

  private final GovernanceActionRepository governanceActionRepository;

  private final PoolHashRepository poolHashRepository;

  private final GovernanceActionMapper governanceActionMapper;

  @Override
  public BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String dRepHashOrPoolHash, GovernanceActionFilter governanceActionFilter, Pageable pageable) {
    BaseFilterResponse<GovernanceActionResponse> govActionResponse = new BaseFilterResponse<>();
    govActionResponse.setData(List.of());

    Long slot = null;
    if (governanceActionFilter.getVoterType().equals(VoterType.DREP_KEY_HASH)) {
      slot = dRepRegistrationRepository.getSlotOfDRepRegistration(dRepHashOrPoolHash);
    } else if (governanceActionFilter.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      slot = poolHashRepository.getSlotNoWhenFirstDelegationByPoolHash(dRepHashOrPoolHash);
    }

    Vote vote =
        governanceActionFilter.getVoteType().equals(VoteType.ANY)
            ? null
            : Vote.valueOf(governanceActionFilter.getVoteType().name());

    org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
        govActionType =
            governanceActionFilter.getActionType().equals(GovActionType.ALL)
                ? null
                : org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .valueOf(governanceActionFilter.getActionType().name());

    Timestamp from =
        Objects.isNull(governanceActionFilter.getFromDate())
            ? null
            : new Timestamp(governanceActionFilter.getFromDate().getTime());

    Timestamp to =
        Objects.isNull(governanceActionFilter.getToDate())
            ? null
            : new Timestamp(governanceActionFilter.getToDate().getTime());
    Page<GovernanceActionProjection> governanceActionProjections =
        governanceActionRepository.getAllByFilter(
            vote, dRepHashOrPoolHash, govActionType, from, to, slot, pageable);

    List<GovernanceActionResponse> governanceActionResponses =
        governanceActionProjections.stream()
            .map(
                governanceActionProjection -> {
                  GovernanceActionResponse governanceActionResponse =
                      governanceActionMapper.fromGovernanceActionProjection(
                          governanceActionProjection);
                  governanceActionResponse.setStatus(GovActionStatus.OPEN);
                  return governanceActionResponse;
                })
            .collect(Collectors.toList());

    if (governanceActionResponses.isEmpty()) {
      return govActionResponse;
    }
    govActionResponse.setTotalItems(governanceActionProjections.getTotalElements());
    govActionResponse.setData(governanceActionResponses);
    govActionResponse.setTotalPages(governanceActionProjections.getTotalPages());
    govActionResponse.setCurrentPage(pageable.getPageNumber());
    return govActionResponse;
  }
}
