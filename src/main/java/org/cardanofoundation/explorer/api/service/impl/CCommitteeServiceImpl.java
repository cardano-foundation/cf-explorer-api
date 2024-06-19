package org.cardanofoundation.explorer.api.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.CommitteeStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeMemberResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeOverviewResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeMemberRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.CCommitteeService;
import org.cardanofoundation.explorer.common.entity.enumeration.CommitteeState;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeMember;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeRegistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
public class CCommitteeServiceImpl implements CCommitteeService {

  private final CommitteeMemberRepository committeeMemberRepository;
  private final GovernanceActionRepository governanceActionRepository;
  private final LatestVotingProcedureRepository latestVotingProcedureRepository;
  private final CommitteeRegistrationRepository committeeRegistrationRepository;
  private final EpochParamRepository epochParamRepository;
  private final EpochRepository epochRepository;

  @Override
  public CommitteeOverviewResponse getCommitteeOverview() {
    EpochParam currentEpochParam = epochParamRepository.findCurrentEpochParam();
    long activeMembers =
        committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(
            currentEpochParam.getEpochNo());

    CommitteeState committeeState =
        activeMembers >= currentEpochParam.getCommitteeMinSize().intValue()
            ? CommitteeState.NORMAL
            : CommitteeState.NO_CONFIDENCE;

    // TODO: get activeEpoch from lsv2 once implemented
    Integer activeEpoch =
        committeeMemberRepository.getMinExpireEpochOfActiveMembers(currentEpochParam.getEpochNo())
            - currentEpochParam.getCommitteeMaxTermLength().intValue();

    Long activeEpochSecondTime = epochRepository.getEpochSecondTimeByEpochNo(activeEpoch);

    List<GovActionType> govActionTypeList = new ArrayList<>(Arrays.asList(GovActionType.values()));
    govActionTypeList.removeAll(
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE));

    Long governanceVotes =
        governanceActionRepository.countGovThatAllowedToVoteByBlockTimeGreaterThanAndGovType(
            activeEpochSecondTime, govActionTypeList);

    // TODO: get from lsv2 once implemented
    String proposalPolicy = null;

    // TODO: get from lsv2 once implemented
    Long lastUpdate = activeEpochSecondTime;

    return CommitteeOverviewResponse.builder()
        .currentState(committeeState)
        .proposalPolicy(proposalPolicy)
        .activeMembers(activeMembers)
        .threshold(currentEpochParam.getCcThreshold())
        .governanceVotes(governanceVotes)
        .lastUpdate(lastUpdate == null ? null : new Date(lastUpdate * 1000))
        .build();
  }

  @Override
  public BaseFilterResponse<CommitteeMemberResponse> getCommitteeMembers(Pageable pageable) {
    EpochParam currentEpochParam = epochParamRepository.findCurrentEpochParam();

    Page<CommitteeMember> committeeMemberPages = committeeMemberRepository.findAll(pageable);
    List<String> coldKeys = committeeMemberPages.map(CommitteeMember::getHash).getContent();

    Map<String, String> hotKeyMap =
        committeeRegistrationRepository.getHotKeyOfCommitteeMemberByColdKeyIn(coldKeys).stream()
            .collect(
                Collectors.toMap(
                    CommitteeRegistration::getColdKey, CommitteeRegistration::getHotKey));

    Page<CommitteeMemberResponse> committeeMemberResponses =
        committeeMemberPages.map(
            committeeMember -> {
              CommitteeMemberResponse committeeMemberResponse = new CommitteeMemberResponse();
              committeeMemberResponse.setScriptHash(committeeMember.getHash());
              committeeMemberResponse.setPublicKey(hotKeyMap.get(committeeMember.getHash()));
              committeeMemberResponse.setStatus(
                  committeeMember.getExpiredEpoch() > currentEpochParam.getEpochNo()
                      ? CommitteeStatus.ACTIVE
                      : CommitteeStatus.EXPIRED);
              committeeMemberResponse.setExpiredEpoch(committeeMember.getExpiredEpoch());
              // TODO: get from lsv2 once implemented
              committeeMemberResponse.setActiveEpoch(
                  committeeMember.getExpiredEpoch()
                      - currentEpochParam.getCommitteeMaxTermLength().intValue());
              return committeeMemberResponse;
            });

    return new BaseFilterResponse<>(committeeMemberResponses);
  }

  @Override
  public CommitteeMemberResponse getCommitteeMemberDetail(String publicKey) {
    CommitteeMember committeeMember =
        committeeMemberRepository
            .findById(publicKey)
            .orElseThrow(() -> new BusinessException(BusinessCode.COMMITTEE_MEMBER_NOT_FOUND));

    String hotKey = committeeMemberRepository.getHotKeyOfCommitteeMemberByColdKey(publicKey);
    EpochParam currentEpochParam = epochParamRepository.findCurrentEpochParam();

    // TODO: get activeEpoch from lsv2 once implemented
    Integer activeEpoch =
        committeeMemberRepository.getMinExpireEpochOfActiveMembers(currentEpochParam.getEpochNo())
            - currentEpochParam.getCommitteeMaxTermLength().intValue();
    Long activeEpochSecondTime = epochRepository.getEpochSecondTimeByEpochNo(activeEpoch);

    Long votedGovCount =
        latestVotingProcedureRepository.countVoteByVoterHash(hotKey, activeEpochSecondTime);

    List<GovActionType> govActionTypeList = new ArrayList<>(Arrays.asList(GovActionType.values()));
    govActionTypeList.removeAll(
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE));

    Long govAllowedToVote =
        governanceActionRepository.countGovThatAllowedToVoteByBlockTimeGreaterThanAndGovType(
            activeEpochSecondTime, govActionTypeList);
    Float votingParticipation =
        govAllowedToVote == 0 ? 0 : (float) votedGovCount / govAllowedToVote;

    return CommitteeMemberResponse.builder()
        .scriptHash(committeeMember.getHash())
        .publicKey(hotKey)
        .status(
            committeeMember.getExpiredEpoch() > currentEpochParam.getEpochNo()
                ? CommitteeStatus.ACTIVE
                : CommitteeStatus.EXPIRED)
        .registeredAt(new Date(activeEpochSecondTime * 1000))
        .termDuration(currentEpochParam.getCommitteeMaxTermLength().intValue())
        .votingParticipation(votingParticipation)
        .build();
  }
}
