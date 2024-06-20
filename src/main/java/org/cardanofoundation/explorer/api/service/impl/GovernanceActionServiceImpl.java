package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolParamGroup;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.ProtocolParamUpdate;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.api.projection.DRepInfoProjection;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.projection.LatestVotingProcedureProjection;
import org.cardanofoundation.explorer.api.projection.PoolOverviewProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeMemberRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.StakeAddressBalanceRepository;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.api.util.ProtocolParamUtil;
import org.cardanofoundation.explorer.common.entity.enumeration.CommitteeState;
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

  private final StakeAddressBalanceRepository stakeAddressBalanceRepository;

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
  private final CommitteeRegistrationRepository committeeRegistrationRepository;
  private final DelegationRepository delegationRepository;
  private final ProtocolParamService protocolParamService;
  private final CommitteInfoRepository committeInfoRepository;
  private final CommitteeMemberRepository committeeMemberRepository;

  @Override
  public BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String voterHash, GovernanceActionFilter governanceActionFilter, Pageable pageable) {
    BaseFilterResponse<GovernanceActionResponse> govActionResponse = new BaseFilterResponse<>();
    govActionResponse.setData(List.of());

    List<String> voterHashes =
        getActualVoterHashes(voterHash, governanceActionFilter.getVoterType());

    Long slot = null;
    if (governanceActionFilter.getVoterType().equals(VoterType.DREP_KEY_HASH)
        || governanceActionFilter.getVoterType().equals(VoterType.DREP_SCRIPT_HASH)) {
      slot = dRepRegistrationRepository.getSlotOfDRepRegistration(voterHashes.get(0));
    } else if (governanceActionFilter.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      slot = poolHashRepository.getSlotNoWhenFirstDelegationByPoolHash(voterHashes.get(0));
    } else if (governanceActionFilter
            .getVoterType()
            .equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH)
        || governanceActionFilter
            .getVoterType()
            .equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH)) {
      if (StringUtils.isEmpty(voterHash)) {
        slot = committeeMemberRepository.getMinSlotOfCommitteeMembers();
      } else {
        slot = committeeMemberRepository.getSlotOfCommitteeMemberByHotKey(voterHash);
      }
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
            voterHashes,
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

  private List<String> getActualVoterHashes(String voterHash, VoterType voterType) {
    List<String> voteHashes = new ArrayList<>();
    if (StringUtils.isEmpty(voterHash)
        && (voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH)
            || voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH))) {
      return committeeMemberRepository.getHotKeyOfCommitteeMember();
    }

    if (voterHash.toLowerCase().startsWith("pool")) {
      voteHashes.add(
          poolHashRepository
              .getHashRawByView(voterHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND)));
    } else if (voterHash.toLowerCase().startsWith("drep")) {
      DRepInfo dRepInfo =
          drepInfoRepository
              .findByDRepHashOrDRepId(voterHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));
      voteHashes.add(dRepInfo.getDrepHash());
    } else {
      voteHashes.add(voterHash);
    }
    return voteHashes;
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

      if (voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH)
          || voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH)) {
        govActionTypeList.removeAll(
            List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE));
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
      String voterHash, GovernanceActionRequest governanceActionRequest) {
    Optional<GovActionDetailsProjection> govActionDetailsProjections =
        governanceActionRepository.getGovActionDetailsByTxHashAndIndex(
            governanceActionRequest.getTxHash(), governanceActionRequest.getIndex());

    if (govActionDetailsProjections.isEmpty()) {
      throw new BusinessException(BusinessCode.GOVERNANCE_ACTION_NOT_FOUND);
    }
    GovActionType govActionType = govActionDetailsProjections.get().getType();

    // STAKING POOL not allowed to vote on treasury withdrawals, parameter change and update
    // committee
    List<GovActionType> govActionTypeListAllowedVoteBySPO =
        List.of(
            GovActionType.TREASURY_WITHDRAWALS_ACTION,
            GovActionType.PARAMETER_CHANGE_ACTION,
            GovActionType.NEW_CONSTITUTION);

    List<GovActionType> govActionTypeListAllowedVoteByCc =
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE);
    Boolean allowedVoteBySPO = !govActionTypeListAllowedVoteBySPO.contains(govActionType);
    Boolean allowedVoteByCC = !govActionTypeListAllowedVoteByCc.contains(govActionType);
    if (voterHash.toLowerCase().startsWith("pool")) {
      voterHash =
          poolHashRepository
              .getHashRawByView(voterHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND));
    } else if (voterHash.toLowerCase().startsWith("drep")) {
      DRepInfo dRepInfo =
          drepInfoRepository
              .findByDRepHashOrDRepId(voterHash)
              .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));
      voterHash = dRepInfo.getDrepHash();
    }

    if (governanceActionRequest.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)
        && govActionTypeListAllowedVoteBySPO.contains(govActionType)) {
      return GovernanceActionDetailsResponse.builder()
          .allowedVoteBySPO(allowedVoteBySPO)
          .allowedVoteByCC(allowedVoteByCC)
          .build();
    }
    GovernanceActionDetailsResponse response =
        governanceActionMapper.fromGovActionDetailsProjection(govActionDetailsProjections.get());
    response.setAllowedVoteByCC(allowedVoteByCC);
    response.setAllowedVoteBySPO(allowedVoteBySPO);
    // get pool name for SPO
    if (governanceActionRequest.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      Optional<String> poolName = poolHashRepository.getPoolNameByPoolHashOrPoolView(voterHash);
      response.setPoolName(poolName.orElse(null));
    }

    List<VoterType> voterTypes = new ArrayList<>();

    if (VoterType.DREP_KEY_HASH.equals(governanceActionRequest.getVoterType())) {
      voterTypes.add(VoterType.DREP_KEY_HASH);
      voterTypes.add(VoterType.DREP_SCRIPT_HASH);
    } else if (VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH.equals(
        governanceActionRequest.getVoterType())) {
      voterTypes.add(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH);
      voterTypes.add(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH);
    } else if (VoterType.STAKING_POOL_KEY_HASH.equals(governanceActionRequest.getVoterType())) {
      voterTypes.add(VoterType.STAKING_POOL_KEY_HASH);
    }

    List<VotingProcedureProjection> votingProcedureProjections =
        votingProcedureRepository.getVotingProcedureByTxHashAndIndexAndVoterHash(
            governanceActionRequest.getTxHash(),
            governanceActionRequest.getIndex(),
            voterHash,
            voterTypes);
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
  public VotingChartResponse getVotingChartByGovActionTxHashAndIndex(
      String txHash, Integer index, VoterType voterType) {

    GovActionDetailsProjection govActionProjection =
        governanceActionRepository
            .getGovActionDetailsByTxHashAndIndex(txHash, index)
            .orElseThrow(() -> new BusinessException(BusinessCode.GOVERNANCE_ACTION_NOT_FOUND));

    EpochParam epochParam = epochParamRepository.findByEpochNo(govActionProjection.getEpoch());

    int expiredEpoch =
        govActionProjection.getEpoch() + getGovActionLifetime(epochParam.getGovActionLifetime());

    Integer committeeTotalCount =
        committeeRegistrationRepository.countByExpiredEpochNo(expiredEpoch);
    CommitteeState committeeState =
        epochParam.getCommitteeMinSize() == null
                || committeeTotalCount >= epochParam.getCommitteeMinSize().intValue()
            ? CommitteeState.CONFIDENCE
            : CommitteeState.NO_CONFIDENCE;

    VotingChartResponse votingChartResponse =
        VotingChartResponse.builder().txHash(txHash).index(index).build();

    switch (voterType) {
      case DREP_KEY_HASH:
        getVotingChartResponseForDRep(
            votingChartResponse, epochParam, govActionProjection, committeeState);
        return votingChartResponse;
      case STAKING_POOL_KEY_HASH:
        getVotingChartResponseForPool(
            votingChartResponse, epochParam, govActionProjection, committeeState);
        return votingChartResponse;
      case CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH:
        getVotingChartResponseForCCType(
            votingChartResponse, epochParam, govActionProjection, committeeState);
        return votingChartResponse;
      default:
        return votingChartResponse;
    }
  }

  @Override
  public BaseFilterResponse<GovernanceActionResponse> getGovCommitteeStatusHistory(
      GovCommitteeHistoryFilter govCommitteeHistoryFilter, Pageable pageable) {

    List<GovActionType> govActionTypeList = new ArrayList<>();
    if (govCommitteeHistoryFilter.getActionType().equals(GovActionType.ALL)) {
      govActionTypeList = List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE);
    } else {
      govActionTypeList = List.of(govCommitteeHistoryFilter.getActionType());
    }

    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);

    if (Objects.nonNull(govCommitteeHistoryFilter.getFromDate())) {
      fromDate = govCommitteeHistoryFilter.getFromDate().toEpochSecond(ZoneOffset.UTC);
    }
    if (Objects.nonNull(govCommitteeHistoryFilter.getToDate())) {
      long to = govCommitteeHistoryFilter.getToDate().toEpochSecond(ZoneOffset.UTC);
      toDate = Math.min(to, toDate);
    }

    String anchorText =
        govCommitteeHistoryFilter.getAnchorText() == null
            ? null
            : govCommitteeHistoryFilter.getAnchorText().toLowerCase();

    Page<GovernanceActionResponse> governanceActionProjections =
        governanceActionRepository
            .getAllGovCommitteeHistory(
                govActionTypeList,
                fromDate,
                toDate,
                govCommitteeHistoryFilter.getGovernanceActionTxHash(),
                anchorText,
                pageable)
            .map(governanceActionMapper::fromGovernanceActionProjection);

    return new BaseFilterResponse<>(governanceActionProjections);
  }

  // TODO: Active vote stake of Pool type is not available.
  private void getVotingChartResponseForPool(
      VotingChartResponse votingChartResponse,
      EpochParam epochParam,
      GovActionDetailsProjection govActionDetailsProjection,
      CommitteeState committeeState) {

    switch (govActionDetailsProjection.getType()) {
      case NO_CONFIDENCE:
        votingChartResponse.setThreshold(epochParam.getPvtMotionNoConfidence());
        break;
      case UPDATE_COMMITTEE:
        votingChartResponse.setThreshold(
            CommitteeState.CONFIDENCE.equals(committeeState)
                ? epochParam.getPvtCommitteeNormal()
                : epochParam.getPvtCommitteeNoConfidence());
        break;
      case HARD_FORK_INITIATION_ACTION:
        votingChartResponse.setThreshold(epochParam.getPvtHardForkInitiation());
        break;
      case INFO_ACTION:
        /* CIP 1694
         * The two thresholds for the Info action are set to 100% since setting it any lower would result in not being able to poll above the threshold. */
        votingChartResponse.setThreshold(1.0);
        break;
      default:
        votingChartResponse.setThreshold(null);
    }
    List<PoolOverviewProjection> poolCanVoteList =
        poolHashRepository.getSlotCreatedAtGroupByPoolHash(govActionDetailsProjection.getSlot());

    Map<String, Long> poolHashByPoolIdMap =
        poolCanVoteList.stream()
            .collect(
                Collectors.toMap(
                    PoolOverviewProjection::getPoolHash, PoolOverviewProjection::getPoolId));

    BigInteger totalActiveVoteStake = getTotalActiveStakeByPoolIds(poolHashByPoolIdMap.keySet());

    List<LatestVotingProcedureProjection> latestVotingProcedureProjections =
        latestVotingProcedureRepository.findByGovActionTxHashAndGovActionIndex(
            govActionDetailsProjection.getTxHash(),
            govActionDetailsProjection.getIndex(),
            poolHashByPoolIdMap.keySet().stream().toList(),
            List.of(VoterType.STAKING_POOL_KEY_HASH));

    Map<Vote, List<LatestVotingProcedureProjection>> voteCount =
        latestVotingProcedureProjections.stream()
            .collect(Collectors.groupingBy(LatestVotingProcedureProjection::getVote));

    Map<Vote, BigInteger> voteStake =
        voteCount.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    entry ->
                        getTotalActiveStakeByPoolIds(
                            entry.getValue().stream()
                                .map(LatestVotingProcedureProjection::getVoterHash)
                                .collect(Collectors.toList()))));

    votingChartResponse.setActiveVoteStake(totalActiveVoteStake);
    votingChartResponse.setTotalYesVoteStake(voteStake.getOrDefault(Vote.YES, BigInteger.ZERO));
    votingChartResponse.setTotalNoVoteStake(voteStake.getOrDefault(Vote.NO, BigInteger.ZERO));
    votingChartResponse.setAbstainVoteStake(voteStake.getOrDefault(Vote.ABSTAIN, BigInteger.ZERO));
  }

  private BigInteger getTotalActiveStakeByPoolIds(Collection<String> poolIds) {
    Set<String> stakeView = delegationRepository.getStakeAddressDelegatorsByPoolIds(poolIds);
    return stakeAddressBalanceRepository.sumBalanceByStakeAddressIn(stakeView);
  }

  // TODO: Active vote stake of DRep type is not available.
  private void getVotingChartResponseForDRep(
      VotingChartResponse votingChartResponse,
      EpochParam epochParam,
      GovActionDetailsProjection govActionDetailsProjection,
      CommitteeState committeeState) {
    switch (govActionDetailsProjection.getType()) {
      case NO_CONFIDENCE:
        votingChartResponse.setThreshold(epochParam.getDvtMotionNoConfidence());
        break;
      case UPDATE_COMMITTEE:
        votingChartResponse.setThreshold(
            CommitteeState.CONFIDENCE.equals(committeeState)
                ? epochParam.getDvtCommitteeNormal()
                : epochParam.getDvtCommitteeNoConfidence());
        break;
      case NEW_CONSTITUTION:
        votingChartResponse.setThreshold(epochParam.getDvtUpdateToConstitution());
        break;
      case HARD_FORK_INITIATION_ACTION:
        votingChartResponse.setThreshold(epochParam.getDvtHardForkInitiation());
        break;
      case PARAMETER_CHANGE_ACTION:
        votingChartResponse.setThreshold(
            getParameterChangeActionThreshold(epochParam, govActionDetailsProjection.getDetails()));
        break;
      case TREASURY_WITHDRAWALS_ACTION:
        votingChartResponse.setThreshold(epochParam.getDvtTreasuryWithdrawal());
        break;
      case INFO_ACTION:
        /* CIP 1694
         * The two thresholds for the Info action are set to 100% since setting it any lower would result in not being able to poll above the threshold. */
        votingChartResponse.setThreshold(1.0);
        break;
      default:
        votingChartResponse.setThreshold(null);
    }

    List<DRepInfoProjection> dRepInfoProjections =
        drepInfoRepository.findDRepByCreatedAt(govActionDetailsProjection.getBlockTime());
    Map<String, BigInteger> activeVoteStakeByDRepMap =
        dRepInfoProjections.stream()
            .collect(
                Collectors.toMap(
                    DRepInfoProjection::getDrepHash, DRepInfoProjection::getActiveVoteStake));
    BigInteger totalActiveVoteStake =
        dRepInfoProjections.stream()
            .map(DRepInfoProjection::getActiveVoteStake)
            .filter(Objects::nonNull)
            .reduce(BigInteger::add)
            .orElse(BigInteger.ZERO);

    List<String> dRepHashes =
        dRepInfoProjections.stream().map(DRepInfoProjection::getDrepHash).toList();

    List<LatestVotingProcedureProjection> latestVotingProcedureProjections =
        latestVotingProcedureRepository.findByGovActionTxHashAndGovActionIndex(
            govActionDetailsProjection.getTxHash(),
            govActionDetailsProjection.getIndex(),
            dRepHashes,
            List.of(VoterType.DREP_KEY_HASH, VoterType.DREP_SCRIPT_HASH));

    Map<Vote, List<LatestVotingProcedureProjection>> voteCount =
        latestVotingProcedureProjections.stream()
            .collect(Collectors.groupingBy(LatestVotingProcedureProjection::getVote));

    Map<Vote, BigInteger> voteStake =
        voteCount.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    entry ->
                        entry.getValue().stream()
                            .map( // get active vote stake of DRep
                                latestVotingProcedureProjection ->
                                    activeVoteStakeByDRepMap.getOrDefault(
                                        latestVotingProcedureProjection.getVoterHash(),
                                        BigInteger.ZERO))
                            .filter(Objects::nonNull)
                            .reduce(BigInteger::add)
                            .orElse(BigInteger.ZERO)));

    votingChartResponse.setActiveVoteStake(totalActiveVoteStake);
    votingChartResponse.setTotalYesVoteStake(voteStake.getOrDefault(Vote.YES, BigInteger.ZERO));
    votingChartResponse.setTotalNoVoteStake(voteStake.getOrDefault(Vote.NO, BigInteger.ZERO));
    votingChartResponse.setAbstainVoteStake(voteStake.getOrDefault(Vote.ABSTAIN, BigInteger.ZERO));
  }

  // TODO: Threshold for CC type is not available. It must be null for now.
  private void getVotingChartResponseForCCType(
      VotingChartResponse votingChartResponse,
      EpochParam epochParam,
      GovActionDetailsProjection govActionDetailsProjection,
      CommitteeState committeeState) {
    List<GovActionType> govActionTypesThatNotAllowedVoteByCC =
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE);
    if (govActionTypesThatNotAllowedVoteByCC.contains(govActionDetailsProjection.getType())) {
      votingChartResponse.setThreshold(null);
    } else {
      Double threshold = protocolParamService.getCCThresholdFromConwayGenesis();
      votingChartResponse.setThreshold(Objects.isNull(threshold) ? null : threshold);
    }
    Long count =
        committeInfoRepository.countByCreatedAtLessThan(govActionDetailsProjection.getBlockTime());
    List<LatestVotingProcedureProjection> latestVotingProcedureProjections =
        latestVotingProcedureRepository.getLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
            govActionDetailsProjection.getTxHash(),
            govActionDetailsProjection.getIndex(),
            List.of(
                VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH,
                VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH),
            govActionDetailsProjection.getBlockTime());

    Map<Vote, Long> voteCount =
        latestVotingProcedureProjections.stream()
            .collect(
                Collectors.groupingBy(
                    LatestVotingProcedureProjection::getVote, Collectors.counting()));
    votingChartResponse.setYesCcMembers(voteCount.getOrDefault(Vote.YES, 0L));
    votingChartResponse.setNoCcMembers(voteCount.getOrDefault(Vote.NO, 0L));
    votingChartResponse.setAbstainCcMembers(voteCount.getOrDefault(Vote.ABSTAIN, 0L));
    votingChartResponse.setCcMembers(count);
  }

  private int getGovActionLifetime(BigInteger govActionLifetime) {
    return govActionLifetime == null ? 0 : govActionLifetime.intValue();
  }

  private Double getParameterChangeActionThreshold(EpochParam epochParam, JsonNode description) {
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode protocolParamUpdateJsonNode = description.get("protocolParamUpdate");
    try {
      ProtocolParamUpdate protocolParamUpdate =
          objectMapper.treeToValue(protocolParamUpdateJsonNode, ProtocolParamUpdate.class);
      List<ProtocolParamGroup> protocolParamGroups =
          ProtocolParamUtil.getGroupsWithNonNullField(protocolParamUpdate);

      Set<Double> doubleSet = new HashSet<>();
      if (protocolParamGroups.contains(ProtocolParamGroup.ECONOMIC)) {
        doubleSet.add(
            Objects.isNull(epochParam.getDvtPPEconomicGroup())
                ? -1
                : epochParam.getDvtPPEconomicGroup());
      } else if (protocolParamGroups.contains(ProtocolParamGroup.NETWORK)) {
        doubleSet.add(
            Objects.isNull(epochParam.getDvtPPNetworkGroup())
                ? -1
                : epochParam.getDvtPPNetworkGroup());
      } else if (protocolParamGroups.contains(ProtocolParamGroup.TECHNICAL)) {
        doubleSet.add(
            Objects.isNull(epochParam.getDvtPPTechnicalGroup())
                ? -1
                : epochParam.getDvtPPTechnicalGroup());
      } else if (protocolParamGroups.contains(ProtocolParamGroup.GOVERNANCE)) {
        doubleSet.add(
            Objects.isNull(epochParam.getDvtPPGovGroup()) ? -1 : epochParam.getDvtPPGovGroup());
      }
      if (doubleSet.isEmpty()) {
        return null;
      } else {
        Double threshold = doubleSet.stream().max(Double::compare).orElse(null);
        return threshold.equals(-1.0) ? null : threshold;
      }
    } catch (JsonProcessingException e) {
      log.error("Error while parsing protocolParamUpdate from description: {}", description);
    }
    return null;
  }
}
