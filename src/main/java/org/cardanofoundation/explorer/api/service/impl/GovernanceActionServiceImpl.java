package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.bloxbean.cardano.client.crypto.Blake2bUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolParamGroup;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.mapper.LatestVotingProcedureMapper;
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.ProtocolParamUpdate;
import org.cardanofoundation.explorer.api.model.dto.GovActionMetaData;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.request.governanceAction.VoteFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.*;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.repository.ledgersync.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.StakeAddressBalanceRepository;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.api.util.ProtocolParamUtil;
import org.cardanofoundation.explorer.common.entity.enumeration.CommitteeState;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.ledgersync.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;
import org.cardanofoundation.explorer.common.exception.BusinessException;
import org.cardanofoundation.explorer.common.utils.HexUtil;

@Service
@RequiredArgsConstructor
@Log4j2
public class GovernanceActionServiceImpl implements GovernanceActionService {

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final GovernanceActionRepository governanceActionRepository;
  private final PoolHashRepository poolHashRepository;
  private final GovernanceActionMapper governanceActionMapper;
  private final VotingProcedureMapper votingProcedureMapper;
  private final VotingProcedureRepository votingProcedureRepository;
  private final DrepInfoRepository drepInfoRepository;
  private final EpochParamRepository epochParamRepository;
  private final LatestVotingProcedureRepository latestVotingProcedureRepository;
  private final DelegationRepository delegationRepository;
  private final ProtocolParamService protocolParamService;
  private final CommitteeMemberRepository committeeMemberRepository;
  private final StakeAddressBalanceRepository stakeAddressBalanceRepository;
  private final OffChainVoteGovActionDataRepository offChainVoteGovActionDataRepository;
  private final LatestVotingProcedureMapper latestVotingProcedureMapper;

  private final RedisTemplate<String, Integer> redisTemplate;

  @Value("${application.network}")
  private String network;

  @Value("${application.epoch.days}")
  public long epochDays;

  public static final String MIN_TIME = "1970-01-01 00:00:00";

  @Override
  public BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String voterHash, GovernanceActionFilter governanceActionFilter, Pageable pageable) {
    List<String> voterHashes =
        getActualVoterHashes(voterHash, governanceActionFilter.getVoterType());
    Long slot = getSlotCheckpoint(voterHash, governanceActionFilter.getVoterType(), voterHashes);
    Boolean isVoteNone = Vote.NONE.equals(governanceActionFilter.getVoteType());

    Vote vote =
        Vote.ANY.equals(governanceActionFilter.getVoteType())
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
        GovActionStatus.ANY.equals(governanceActionFilter.getActionStatus())
                || governanceActionFilter.getActionStatus() == null
            ? null
            : org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus.valueOf(
                governanceActionFilter.getActionStatus().name());

    String anchorText =
        governanceActionFilter.getAnchorText() == null
            ? null
            : governanceActionFilter.getAnchorText().toLowerCase();

    Page<GovernanceActionProjection> governanceActionProjectionPage;

    // get gov info include voter info
    if (!CollectionUtils.isEmpty(voterHashes)) {
      governanceActionProjectionPage =
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
    } else { // get gov info only
      governanceActionProjectionPage =
          governanceActionRepository.getAllByFilter(
              govActionStatus,
              govActionTypeList,
              fromDate,
              toDate,
              governanceActionFilter.getGovernanceActionTxHash(),
              anchorText,
              pageable);
    }

    List<GovernanceActionResponse> governanceActionResponses =
        governanceActionProjectionPage.stream()
            .map(governanceActionMapper::fromGovernanceActionProjection)
            .collect(Collectors.toList());

    return new BaseFilterResponse<>(governanceActionProjectionPage, governanceActionResponses);
  }

  private Long getSlotCheckpoint(String voterHash, VoterType voterType, List<String> voterHashes) {
    Long slot = null;
    if (voterType == null) {
      return slot;
    }

    if (voterType.equals(VoterType.DREP_KEY_HASH) || voterType.equals(VoterType.DREP_SCRIPT_HASH)) {
      slot = dRepRegistrationRepository.getSlotOfDRepRegistration(voterHashes.get(0));
    } else if (voterType.equals(VoterType.STAKING_POOL_KEY_HASH)) {
      slot = poolHashRepository.getSlotNoWhenFirstDelegationByPoolHash(voterHashes.get(0));
    } else if (voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH)
        || voterType.equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH)) {
      if (StringUtils.isEmpty(voterHash)) {
        slot = committeeMemberRepository.getMinSlotOfCommitteeMembers();
      } else {
        slot = committeeMemberRepository.getSlotOfCommitteeMemberByHotKey(voterHash);
      }
    }
    return slot;
  }

  private List<String> getActualVoterHashes(String voterHash, VoterType voterType) {
    List<String> voteHashes = new ArrayList<>();
    if (voterType == null) {
      return voteHashes;
    }
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

    if (voterType == null) {
      return govActionTypeList;
    }

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

    Boolean allowedVoteBySPO = isAllowedVoteBySPO(govActionType);
    Boolean allowedVoteByCC = isAllowedVoteByCC(govActionType);
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
        && !allowedVoteBySPO) {
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

  // STAKING POOL not allowed to vote on treasury withdrawals, parameter change and update
  private boolean isAllowedVoteBySPO(GovActionType govActionType) {
    List<GovActionType> govActionTypeListAllowedVoteBySPO =
        List.of(
            GovActionType.TREASURY_WITHDRAWALS_ACTION,
            GovActionType.PARAMETER_CHANGE_ACTION,
            GovActionType.NEW_CONSTITUTION);
    return !govActionTypeListAllowedVoteBySPO.contains(govActionType);
  }

  private boolean isAllowedVoteByCC(GovActionType govActionType) {
    List<GovActionType> govActionTypeListAllowedVoteByCc =
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE);
    return !govActionTypeListAllowedVoteByCc.contains(govActionType);
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
    EpochParam currentEpochParam = epochParamRepository.findCurrentEpochParam();
    long activeMembers =
        committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(
            currentEpochParam.getEpochNo());

    CommitteeState committeeState =
        epochParam.getCommitteeMinSize() == null
                || activeMembers >= epochParam.getCommitteeMinSize().intValue()
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
            votingChartResponse, epochParam, govActionProjection, activeMembers);
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

  @Override
  public GovernanceOverviewResponse getGovernanceOverview() {
    EpochParam currentEpochParam = epochParamRepository.findCurrentEpochParam();

    Long activeDReps = drepInfoRepository.countByStatus(DRepStatus.ACTIVE);
    Long activeSPOs =
        Objects.requireNonNull(
                redisTemplate.opsForValue().get(CommonConstant.REDIS_POOL_ACTIVATE + network))
            .longValue();
    Long activeCommittees =
        committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(
            currentEpochParam.getEpochNo());

    Map<GovActionType, Long> govActionTypeMap =
        governanceActionRepository.getGovActionGroupByGovActionType().stream()
            .collect(
                Collectors.toMap(
                    GovernanceActionProjection::getType, GovernanceActionProjection::getGovCount));

    Map<GovActionStatus, Long> govActionStatusMap =
        governanceActionRepository.getGovActionGroupByGovActionStatus().stream()
            .collect(
                Collectors.toMap(
                    GovernanceActionProjection::getStatus,
                    GovernanceActionProjection::getGovCount));

    return GovernanceOverviewResponse.builder()
        .activeDReps(activeDReps)
        .activeSPOs(activeSPOs)
        .activeCommittees(activeCommittees)
        .totalGovActions(govActionTypeMap.values().stream().reduce(0L, Long::sum))
        .govCountMap(govActionTypeMap)
        .govStatusMap(govActionStatusMap)
        .build();
  }

  @Override
  public GovernanceActionOverViewResponse getGovernanceActionInfo(String txHash, Integer index) {

    GovernanceActionOverviewProjection governanceActionOverviewProjection =
        governanceActionRepository.getGovernanceActionOverviewByTxHashAndIndex(txHash, index);
    GovernanceActionOverViewResponse response =
        governanceActionMapper.fromGovernanceActionOverviewProjection(
            governanceActionOverviewProjection);
    GovActionType type = response.getActionType();
    if (Objects.nonNull(governanceActionOverviewProjection.getRawData())
        && Objects.nonNull(governanceActionOverviewProjection.getAnchorHash())) {
      String hash =
          HexUtil.encodeHexString(
              Blake2bUtil.blake2bHash256(
                  governanceActionOverviewProjection.getRawData().getBytes()));

      if (!hash.equals(governanceActionOverviewProjection.getAnchorHash())) {
        response.setIsValidHash(false);
      } else {
        response.setIsValidHash(true);
      }
    } else {
      response.setIsValidHash(false);
    }
    response.setAllowedVoteByCC(isAllowedVoteByCC(type));
    response.setAllowedVoteBySPO(isAllowedVoteBySPO(type));
    return response;
  }

  @Override
  public BaseFilterResponse<AuthorResponse> getAuthorsByAnchor(
      String anchorUrl, String anchorHash, Pageable pageable) {
    String rawData =
        offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anchorUrl, anchorHash);
    if (Objects.isNull(rawData)) {
      return new BaseFilterResponse<>(Page.empty(pageable));
    }
    ObjectMapper mapper = new ObjectMapper();
    // ensure that: the field which missing in the json string will not throw exception
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    try {
      GovActionMetaData govActionMetaData = mapper.readValue(rawData, GovActionMetaData.class);
      if (govActionMetaData.getAuthors() == null || govActionMetaData.getAuthors().isEmpty()) {
        return new BaseFilterResponse<>(Page.empty(pageable));
      }
      List<AuthorResponse> authorResponses =
          govActionMetaData.getAuthors().stream()
              .map(
                  author ->
                      AuthorResponse.builder()
                          .name(author.getName())
                          .publicKey(author.getWitness().getPublicKey())
                          .witnessAlgorithm(author.getWitness().getWitnessAlgorithm())
                          .signature(author.getWitness().getSignature())
                          .build())
              .collect(Collectors.toList());
      authorResponses.sort(Comparator.comparing(AuthorResponse::getName));
      return new BaseFilterResponse<>(BaseFilterResponse.getPageImpl(authorResponses, pageable));
    } catch (JsonProcessingException e) {
      log.error("Error when parsing raw data to GovActionMetaData: {}", e.getMessage());
      return new BaseFilterResponse<>(Page.empty(pageable));
    }
  }

  @Override
  public BaseFilterResponse<VotingOnGovActionResponse> getVotingOnGovAction(
      VoteFilter voteFilter, Pageable pageable) {
    // if voter type is null, get all type of voter
    if (voteFilter.getVoterType() == null) {
      return getDataWithAnyTypeOrCCType(voteFilter, pageable);
    } else if (voteFilter.getVoterType().equals(VoterType.DREP_KEY_HASH)
        || voteFilter.getVoterType().equals(VoterType.DREP_SCRIPT_HASH)) {
      return getDataWithDRepType(voteFilter, pageable);
    } else {
      return getDataWithAnyTypeOrCCType(voteFilter, pageable);
    }
  }

  @Override
  public RangeFilterVoteResponse getRangeFilterVoteResponse(String txHash, Integer index) {
    DRepRangeProjection dRepRangeProjection =
        latestVotingProcedureRepository.getDRepRangeValuesForVotesFilter(txHash, index);
    return RangeFilterVoteResponse.builder()
        .maxActiveStake(dRepRangeProjection.getMaxActiveVoteStake())
        .minActiveStake(dRepRangeProjection.getMinActiveVoteStake())
        .build();
  }

  private BaseFilterResponse<VotingOnGovActionResponse> getDataWithAnyTypeOrCCType(
      VoteFilter voteFilter, Pageable pageable) {
    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);

    if (Objects.nonNull(voteFilter.getFromDate())) {
      fromDate = voteFilter.getFromDate().toEpochSecond(ZoneOffset.UTC);
    }
    if (Objects.nonNull(voteFilter.getToDate())) {
      long to = voteFilter.getToDate().toEpochSecond(ZoneOffset.UTC);
      toDate = Math.min(to, toDate);
    }

    List<VoterType> voterTypes = new ArrayList<>();

    if (voteFilter.getVoterType() == null) {
      voterTypes.addAll(Arrays.stream(VoterType.values()).toList());
    } else if (voteFilter.getVoterType().equals(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH)) {
      voterTypes.add(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH);
      voterTypes.add(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH);
    } else if (voteFilter.getVoterType().equals(VoterType.STAKING_POOL_KEY_HASH)) {
      voterTypes.add(VoterType.STAKING_POOL_KEY_HASH);
    }

    Page<LatestVotingProcedureProjection> latestVotingProcedureProjections =
        latestVotingProcedureRepository.getVoteOnGovActionByAnyType(
            voteFilter.getTxHash(),
            voteFilter.getIndex(),
            voteFilter.getVoterHash(),
            fromDate,
            toDate,
            voterTypes,
            pageable);

    List<VotingOnGovActionResponse> votingOnGovActionResponses =
        latestVotingProcedureProjections.stream()
            .map(latestVotingProcedureMapper::fromLatestVotingProcedureProjection)
            .toList();

    getActiveStakeForDRep(votingOnGovActionResponses);

    return new BaseFilterResponse<>(latestVotingProcedureProjections, votingOnGovActionResponses);
  }

  private void getActiveStakeForDRep(List<VotingOnGovActionResponse> votingOnGovActionResponses) {
    List<String> drepHashes =
        votingOnGovActionResponses.stream()
            .filter(
                votingOnGovActionResponse ->
                    votingOnGovActionResponse.getVoterType().equals(VoterType.DREP_KEY_HASH)
                        || votingOnGovActionResponse
                            .getVoterType()
                            .equals(VoterType.DREP_SCRIPT_HASH))
            .map(VotingOnGovActionResponse::getVoterHash)
            .collect(Collectors.toList());
    List<DRepInfoProjection> dRepInfoProjections = drepInfoRepository.findByDRepHashIn(drepHashes);
    Map<String, BigInteger> activeVoteStakeMap =
        dRepInfoProjections.stream()
            .collect(
                Collectors.toMap(
                    DRepInfoProjection::getDrepHash, DRepInfoProjection::getActiveVoteStake));
    votingOnGovActionResponses.forEach(
        votingOnGovActionResponse -> {
          votingOnGovActionResponse.setVotingStake(
              activeVoteStakeMap.getOrDefault(votingOnGovActionResponse.getVoterHash(), null));
        });
  }

  private BaseFilterResponse<VotingOnGovActionResponse> getDataWithDRepType(
      VoteFilter voteFilter, Pageable pageable) {

    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);

    if (Objects.nonNull(voteFilter.getFromDate())) {
      fromDate = voteFilter.getFromDate().toEpochSecond(ZoneOffset.UTC);
    }
    if (Objects.nonNull(voteFilter.getToDate())) {
      long to = voteFilter.getToDate().toEpochSecond(ZoneOffset.UTC);
      toDate = Math.min(to, toDate);
    }

    if (voteFilter.getActiveStakeFrom() == null) {
      voteFilter.setActiveStakeFrom(BigInteger.ZERO);
    }

    if (voteFilter.getActiveStakeTo() == null) {
      voteFilter.setActiveStakeTo(BigInteger.valueOf(Long.MAX_VALUE));
    }

    List<VoterType> voterTypes =
        new ArrayList<>(List.of(VoterType.DREP_KEY_HASH, VoterType.DREP_SCRIPT_HASH));

    Page<LatestVotingProcedureProjection> latestVotingProcedureProjections =
        latestVotingProcedureRepository.getVoteOnGovActionByDRepType(
            voteFilter.getTxHash(),
            voteFilter.getIndex(),
            voteFilter.getVoterHash(),
            fromDate,
            toDate,
            voterTypes,
            voteFilter.getActiveStakeFrom(),
            voteFilter.getActiveStakeTo(),
            pageable);

    List<VotingOnGovActionResponse> votingOnGovActionResponses =
        latestVotingProcedureProjections.stream()
            .map(latestVotingProcedureMapper::fromLatestVotingProcedureProjection)
            .toList();

    return new BaseFilterResponse<>(latestVotingProcedureProjections, votingOnGovActionResponses);
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
      Long activeMembers) {
    List<GovActionType> govActionTypesThatNotAllowedVoteByCC =
        List.of(GovActionType.NO_CONFIDENCE, GovActionType.UPDATE_COMMITTEE);
    if (govActionTypesThatNotAllowedVoteByCC.contains(govActionDetailsProjection.getType())) {
      votingChartResponse.setThreshold(null);
    } else {
      Double threshold = epochParam.getCcThreshold();
      if (threshold == null) {
        threshold = protocolParamService.getCCThresholdFromConwayGenesis();
      }
      votingChartResponse.setThreshold(threshold);
    }

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
    votingChartResponse.setCcMembers(activeMembers);
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
