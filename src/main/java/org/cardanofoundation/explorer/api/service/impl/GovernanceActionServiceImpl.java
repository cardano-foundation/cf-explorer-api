package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
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

import org.cardanofoundation.explorer.api.common.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.common.enumeration.VoteType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChart;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.api.projection.CountVoteOnGovActionProjection;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;
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
    }

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

    List<org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType>
        govActionTypeList =
            getGovActionTypeByVoterType(
                governanceActionFilter.getVoterType(), governanceActionFilter.getActionType());

    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate =
        Timestamp.from(
                    LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
                        .toInstant(ZoneOffset.UTC))
                .getTime()
            / 1000;

    if (Objects.nonNull(governanceActionFilter.getFromDate())) {
      fromDate = Timestamp.from(governanceActionFilter.getFromDate().toInstant()).getTime() / 1000;
    }
    if (Objects.nonNull(governanceActionFilter.getToDate())) {
      long to = Timestamp.from(governanceActionFilter.getToDate().toInstant()).getTime() / 1000;
      toDate = Math.min(to, toDate);
    }

    org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus govActionStatus =
        governanceActionFilter.getActionStatus().equals(GovActionStatus.ANY)
            ? null
            : org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus.valueOf(
                governanceActionFilter.getActionStatus().name());

    String anchorText = governanceActionFilter.getAnchorText();

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

  private List<org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType>
      getGovActionTypeByVoterType(VoterType voterType, GovActionType govActionType) {
    List<org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType>
        govActionTypeList =
            new ArrayList<>(
                Arrays.asList(
                    org.cardanofoundation.explorer.common.entity.ledgersync.enumeration
                        .GovActionType.values()));

    if (govActionType.equals(
        org.cardanofoundation.explorer.api.common.enumeration.GovActionType.ALL)) {
      if (voterType.equals(VoterType.STAKING_POOL_KEY_HASH)) {
        govActionTypeList.remove(
            org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                .NEW_CONSTITUTION);
        govActionTypeList.remove(
            org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                .PARAMETER_CHANGE_ACTION);
        govActionTypeList.remove(
            org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                .TREASURY_WITHDRAWALS_ACTION);
      }
    } else {
      govActionTypeList = new ArrayList<>();
      govActionTypeList.add(
          org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType.valueOf(
              govActionType.name()));
      if (voterType.equals(VoterType.STAKING_POOL_KEY_HASH)) {
        govActionTypeList.removeAll(
            List.of(
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .NEW_CONSTITUTION,
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .PARAMETER_CHANGE_ACTION,
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .TREASURY_WITHDRAWALS_ACTION));
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
    }
    org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
        govActionType = govActionDetailsProjections.get().getType();
    // STAKING POOL not allowed to vote on treasury withdrawals, parameter change and update
    // committee
    List<org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType>
        govActionTypes =
            List.of(
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .TREASURY_WITHDRAWALS_ACTION,
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .PARAMETER_CHANGE_ACTION,
                org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .NEW_CONSTITUTION);
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
      response.setVoteType(VoteType.NONE);
      return response;
    }
    List<HistoryVote> historyVotes =
        votingProcedureProjections.stream()
            .sorted(Comparator.comparing(VotingProcedureProjection::getBlockTime).reversed())
            .map(votingProcedureMapper::fromVotingProcedureProjection)
            .toList();
    response.setVoteType(VoteType.valueOf(votingProcedureProjections.get(0).getVote().name()));
    response.setHistoryVotes(historyVotes);
    return response;
  }

  void setExpiryDateOfGovAction(GovernanceActionDetailsResponse response) {
    Epoch epoch =
        epochRepository
            .findFirstByNo(response.getEpoch())
            .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    Epoch firstEpoch =
        epochRepository
            .findFirstByNo(BigInteger.ZERO.intValue())
            .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();
    EpochResponse epochResponse = epochMapper.epochToEpochResponse(epoch);
    LocalDateTime startTime =
        modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epochResponse.getStartTime());
    EpochParam epochParam = epochParamRepository.findByEpochNo(response.getEpoch());
    response.setExpiryDate(
        Timestamp.valueOf(
            startTime.plusDays(epochDays * epochParam.getGovActionLifetime().longValue())));
  }

  private LocalDateTime modifyStartTimeAndEndTimeOfEpoch(
      LocalDateTime firstEpochStartTime, LocalDateTime epochTime) {
    return LocalDateTime.of(
        epochTime.getYear(),
        epochTime.getMonth(),
        epochTime.getDayOfMonth(),
        firstEpochStartTime.getHour(),
        firstEpochStartTime.getMinute(),
        firstEpochStartTime.getSecond());
  }

  @Override
  public VotingChartResponse getVotingChartByGovActionTxHashAndIndex(String txHash, Integer index) {
    List<CountVoteOnGovActionProjection> votingProcedureProjectionList =
        latestVotingProcedureRepository
            .countLatestVotingProcedureByGovActionTxHashAndGovActionIndex(txHash, index);

    Map<Vote, Long> voteCount =
        votingProcedureProjectionList.stream()
            .collect(
                Collectors.groupingBy(
                    CountVoteOnGovActionProjection::getVote,
                    Collectors.summingLong(CountVoteOnGovActionProjection::getCount)));

    long yesVotes = voteCount.getOrDefault(Vote.YES, 0L);
    long noVotes = voteCount.getOrDefault(Vote.NO, 0L);
    long abstainVotes = voteCount.getOrDefault(Vote.ABSTAIN, 0L);

    VotingChartResponse votingChart =
        VotingChartResponse.builder()
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
                        CountVoteOnGovActionProjection::getVote,
                        Collectors.summingLong(CountVoteOnGovActionProjection::getCount))));

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
