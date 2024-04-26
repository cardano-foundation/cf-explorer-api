package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChart;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.api.projection.CountVoteOnGovActionProjection;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class GovernanceActionServiceImpl implements GovernanceActionService {

  @Value("${application.epoch.days}")
  public long epochDays;

  public static final String MIN_TIME = "1970-01-01 00:00:00";

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final GovernanceActionRepository governanceActionRepository;
  private final PoolHashRepository poolHashRepository;
  private final GovernanceActionMapper governanceActionMapper;
  private final VotingProcedureMapper votingProcedureMapper;
  private final VotingProcedureRepository votingProcedureRepository;
  private final DrepInfoRepository drepInfoRepository;
  private final EpochParamRepository epochParamRepository;
  private final LatestVotingProcedureRepository latestVotingProcedureRepository;

  private final EpochMapper epochMapper;
  private final EpochRepository epochRepository;

  @Override
  public BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String dRepHashOrPoolHash, GovernanceActionFilter governanceActionFilter, Pageable pageable) {
    BaseFilterResponse<GovernanceActionResponse> govActionResponse = new BaseFilterResponse<>();
    govActionResponse.setData(List.of());

    if (dRepHashOrPoolHash.toLowerCase().startsWith("pool")) {
      dRepHashOrPoolHash =
          poolHashRepository
              .getHashRawByView(dRepHashOrPoolHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND));
    } else if (dRepHashOrPoolHash.toLowerCase().startsWith("drep")) {
      DRepInfo dRepInfo =
          drepInfoRepository
              .findByDRepHashOrDRepId(dRepHashOrPoolHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));
      dRepHashOrPoolHash = dRepInfo.getDrepHash();
    }

    Long slot = null;
    if (governanceActionFilter.getVoterType().equals(VoterType.DREP_KEY_HASH)) {
      slot = dRepRegistrationRepository.getSlotOfDRepRegistration(dRepHashOrPoolHash);
    } else if (governanceActionFilter.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      slot = poolHashRepository.getSlotNoWhenFirstDelegationByPoolHash(dRepHashOrPoolHash);
    }

    Boolean isVoteNone = governanceActionFilter.getVoteType().equals(Vote.NONE);

    Vote vote =
        governanceActionFilter.getVoteType().equals(Vote.ANY)
            ? null
            : governanceActionFilter.getVoteType();

    List<GovActionType> govActionTypeList =
        getGovActionTypeByVoterType(
            governanceActionFilter.getVoterType(), governanceActionFilter.getActionType());

    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);

    if (Objects.nonNull(governanceActionFilter.getFromDate())) {
      fromDate = governanceActionFilter.getFromDate().toEpochSecond(ZoneOffset.UTC);
    }
    if (Objects.nonNull(governanceActionFilter.getToDate())) {
      long to = governanceActionFilter.getToDate().toEpochSecond(ZoneOffset.UTC);
      toDate = Math.min(to, toDate);
    }

    org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus govActionStatus =
        governanceActionFilter.getActionStatus().equals(GovActionStatus.ANY)
            ? null
            : org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus.valueOf(
                governanceActionFilter.getActionStatus().name());

    String anchorText =
        governanceActionFilter.getAnchorText() == null
            ? null
            : governanceActionFilter.getAnchorText().toLowerCase();

    Page<GovernanceActionProjection> governanceActionProjections =
        governanceActionRepository.getAllByFilter(
            governanceActionFilter.getIsRepeatVote(),
            govActionStatus,
            vote,
            dRepHashOrPoolHash,
            govActionTypeList,
            fromDate,
            toDate,
            slot,
            governanceActionFilter.getGovernanceActionTxHash(),
            anchorText,
            isVoteNone,
            pageable);

    List<GovernanceActionResponse> governanceActionResponses =
        governanceActionProjections.stream()
            .map(governanceActionMapper::fromGovernanceActionProjection)
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

  private List<GovActionType> getGovActionTypeByVoterType(
      VoterType voterType, GovActionType govActionType) {
    List<GovActionType> govActionTypeList = new ArrayList<>(Arrays.asList(GovActionType.values()));

    if (govActionType.equals(GovActionType.ALL)) {
      if (voterType.equals(VoterType.STAKING_POOL_KEY_HASH)) {
        govActionTypeList.remove(GovActionType.NEW_CONSTITUTION);
        govActionTypeList.remove(GovActionType.PARAMETER_CHANGE_ACTION);
        govActionTypeList.remove(GovActionType.TREASURY_WITHDRAWALS_ACTION);
      }
    } else {
      govActionTypeList = new ArrayList<>();
      govActionTypeList.add(GovActionType.valueOf(govActionType.name()));
      if (voterType.equals(VoterType.STAKING_POOL_KEY_HASH)) {
        govActionTypeList.removeAll(
            List.of(
                GovActionType.NEW_CONSTITUTION,
                GovActionType.PARAMETER_CHANGE_ACTION,
                GovActionType.TREASURY_WITHDRAWALS_ACTION));
      }
    }
    return govActionTypeList;
  }

  @Override
  public GovernanceActionDetailsResponse getGovernanceActionDetails(
      String dRepHashOrPoolHashOrPoolView, GovernanceActionRequest governanceActionRequest) {
    Optional<GovActionDetailsProjection> govActionDetailsProjections =
        governanceActionRepository.getGovActionDetailsByTxHashAndIndex(
            governanceActionRequest.getTxHash(), governanceActionRequest.getIndex());
    if (govActionDetailsProjections.isEmpty()) {
      throw new BusinessException(BusinessCode.GOVERNANCE_ACTION_NOT_FOUND);
    }
    if (dRepHashOrPoolHashOrPoolView.toLowerCase().startsWith("pool")) {
      dRepHashOrPoolHashOrPoolView =
          poolHashRepository
              .getHashRawByView(dRepHashOrPoolHashOrPoolView)
              .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND));
    } else if (dRepHashOrPoolHashOrPoolView.toLowerCase().startsWith("drep")) {
      DRepInfo dRepInfo =
          drepInfoRepository
              .findByDRepHashOrDRepId(dRepHashOrPoolHashOrPoolView)
              .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));
      dRepHashOrPoolHashOrPoolView = dRepInfo.getDrepHash();
    }
    GovActionType govActionType = govActionDetailsProjections.get().getType();
    // STAKING POOL not allowed to vote on treasury withdrawals, parameter change and update
    // committee
    List<GovActionType> govActionTypes =
        List.of(
            GovActionType.TREASURY_WITHDRAWALS_ACTION,
            GovActionType.PARAMETER_CHANGE_ACTION,
            GovActionType.NEW_CONSTITUTION);
    if (governanceActionRequest.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)
        && govActionTypes.contains(govActionType)) {
      return new GovernanceActionDetailsResponse();
    }
    GovernanceActionDetailsResponse response =
        governanceActionMapper.fromGovActionDetailsProjection(govActionDetailsProjections.get());
    // get pool name for SPO
    if (governanceActionRequest.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      Optional<String> poolName =
          poolHashRepository.getPoolNameByPoolHashOrPoolView(dRepHashOrPoolHashOrPoolView);
      response.setPoolName(poolName.orElse(null));
    }

    VoterType voterType = VoterType.valueOf(governanceActionRequest.getVoterType().name());

    List<VotingProcedureProjection> votingProcedureProjections =
        votingProcedureRepository.getVotingProcedureByTxHashAndIndexAndVoterHash(
            governanceActionRequest.getTxHash(),
            governanceActionRequest.getIndex(),
            dRepHashOrPoolHashOrPoolView,
            voterType);
    setExpiryDateOfGovAction(response);
    // no vote procedure found = none vote
    if (votingProcedureProjections.isEmpty()) {
      response.setVoteType(Vote.NONE);
      return response;
    }
    List<HistoryVote> historyVotes =
        votingProcedureProjections.stream()
            .map(votingProcedureMapper::fromVotingProcedureProjection)
            .toList();
    response.setVoteType(votingProcedureProjections.get(0).getVote());
    response.setHistoryVotes(historyVotes);
    return response;
  }

  void setExpiryDateOfGovAction(GovernanceActionDetailsResponse response) {
    Instant startTime = Instant.ofEpochSecond(response.getBlockTime());
    EpochParam epochParam = epochParamRepository.findByEpochNo(response.getEpoch());
    Instant expiryTime =
        startTime.plus(
            epochDays * getLongValue(epochParam.getGovActionLifetime()), ChronoUnit.DAYS);
    response.setExpiryDate(Date.from(expiryTime));
  }

  private Long getLongValue(BigInteger bigInteger) {
    return bigInteger == null ? 0L : bigInteger.longValue();
  }

  @Override
  public VotingChartResponse getVotingChartByGovActionTxHashAndIndex(String txHash, Integer index) {
    List<CountVoteOnGovActionProjection> votingProcedureProjectionList =
        latestVotingProcedureRepository.getLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
            txHash, index);

    Map<Vote, List<CountVoteOnGovActionProjection>> voteCount =
        votingProcedureProjectionList.stream()
            .collect(
                Collectors.groupingBy(
                    CountVoteOnGovActionProjection::getVote, Collectors.toList()));

    long yesVotes = voteCount.getOrDefault(Vote.YES, List.of()).size();
    long noVotes = voteCount.getOrDefault(Vote.NO, List.of()).size();
    long abstainVotes = voteCount.getOrDefault(Vote.ABSTAIN, List.of()).size();

    VotingChartResponse votingChart =
        VotingChartResponse.builder()
            .txHash(txHash)
            .index(index)
            .numberOfYesVote(yesVotes)
            .numberOfNoVotes(noVotes)
            .numberOfAbstainVotes(abstainVotes)
            .build();

    List<VotingChart> list = new ArrayList<>();

    Map<VoterType, Map<Vote, Long>> voteCountByVoterTypeAndVote =
        votingProcedureProjectionList.stream()
            .collect(
                Collectors.groupingBy(
                    CountVoteOnGovActionProjection::getVoterType,
                    Collectors.groupingBy(
                        CountVoteOnGovActionProjection::getVote, Collectors.counting())));

    voteCountByVoterTypeAndVote.forEach(
        (voterType, voteCountMap) -> {
          list.add(
              VotingChart.builder()
                  .voterType(voterType)
                  .numberOfYesVote(voteCountMap.getOrDefault(Vote.YES, 0L))
                  .numberOfNoVotes(voteCountMap.getOrDefault(Vote.NO, 0L))
                  .numberOfAbstainVotes(voteCountMap.getOrDefault(Vote.ABSTAIN, 0L))
                  .build());
        });
    votingChart.setVotingChartsList(list);
    return votingChart;
  }
}
