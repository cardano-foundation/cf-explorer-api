package org.cardanofoundation.explorer.api.service.impl;

import java.sql.Timestamp;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
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
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetails;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
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

  private final EpochRepository epochRepository;

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

  @Override
  public GovernanceActionDetailsResponse getGovernanceActionDetails(
      String dRepHashOrPoolHash, GovernanceActionRequest governanceActionRequest) {
    List<GovActionDetailsProjection> govActionDetailsProjections =
        governanceActionRepository.getGovActionDetailsByTxHashAndIndex(
            governanceActionRequest.getTxHash(),
            governanceActionRequest.getIndex(),
            dRepHashOrPoolHash);
    List<GovernanceActionDetails> governanceActionDetails =
        new java.util.ArrayList<>(
            govActionDetailsProjections.stream()
                .map(governanceActionMapper::fromGovActionDetailsProjection)
                .sorted(Comparator.comparing(GovernanceActionDetails::getBlockTime).reversed())
                .toList());
    GovernanceActionDetailsResponse response;
    if (governanceActionDetails.isEmpty()) {
      response =
          governanceActionMapper.fromGovActionDetailsWithHistoryVotes(
              governanceActionDetails.get(0));
      response.setVoteType(VoteType.NONE);
      return response;
    }

    Integer epoch = governanceActionDetails.get(0).getEpoch();

    Timestamp endDate = epochRepository.getEndDateByEpochNo(epoch);

    Optional<GovernanceActionDetails> governanceActionDetailsOptional =
        governanceActionDetails.stream()
            .max(Comparator.comparing(GovernanceActionDetails::getBlockTime));

    response =
        governanceActionMapper.fromGovActionDetailsWithHistoryVotes(
            governanceActionDetailsOptional.get());
    response.setExpiryDate(new Date(endDate.getTime()));
    response.setHistoryVotes(
        governanceActionDetails.stream()
            .map(
                governanceActionDetail ->
                    HistoryVote.builder()
                        .vote(Vote.valueOf(governanceActionDetail.getVoteType().toString()))
                        .timestamp(
                            governanceActionDetail.getBlockTime() == null
                                ? null
                                : new Date(governanceActionDetail.getBlockTime() * 1000))
                        .build())
            .collect(Collectors.toList()));
    return response;
  }
}
