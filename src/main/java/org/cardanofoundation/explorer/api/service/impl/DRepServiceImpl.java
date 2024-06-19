package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.mapper.DRepMapper;
import org.cardanofoundation.explorer.api.model.request.drep.DRepFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepOverviewResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepStatusCountProjection;
import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.DRepRangeProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationVoteRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.DRepService;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class DRepServiceImpl implements DRepService {

  public static final String MIN_TIME = "1970-01-01 00:00:00";

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final DRepCertificateMapper dRepCertificateMapper;
  private final VotingProcedureRepository votingProcedureRepository;
  private final DrepInfoRepository drepInfoRepository;
  private final DelegationVoteRepository delegationVoteRepository;
  private final FetchRewardDataService fetchRewardDataService;
  private final LatestVotingProcedureRepository latestVotingProcedureRepository;
  private final GovernanceActionRepository governanceActionRepository;
  private final DRepMapper dRepMapper;

  private final EpochService epochService;

  @Override
  public BaseFilterResponse<DRepCertificateHistoryResponse> getTxDRepCertificateHistory(
      String drepHashOrDrepId, Pageable pageable) {
    List<DRepCertificateProjection> dRepCertificateProjections =
        dRepRegistrationRepository.getDRepCertificateByDRepIdOrHash(drepHashOrDrepId);
    List<DRepCertificateHistoryResponse> dRepCertificateHistoryResponses =
        dRepCertificateProjections.stream()
            .collect(Collectors.groupingBy(DRepCertificateProjection::getTxHash))
            .values()
            .stream()
            .map(
                dRepCertificateProjectionList -> {
                  DRepCertificateHistoryResponse dRepCertificateHistoryResponse;
                  dRepCertificateHistoryResponse =
                      dRepCertificateMapper.fromDRepCertProjection(
                          dRepCertificateProjectionList.get(0));
                  dRepCertificateHistoryResponse.setActionTypes(
                      dRepCertificateProjectionList.stream()
                          .map(DRepCertificateProjection::getType)
                          .toList());
                  return dRepCertificateHistoryResponse;
                })
            .sorted(
                Sort.Direction.DESC.equals(
                        pageable.getSort().getOrderFor("createdAt").getDirection())
                    ? Comparator.comparing(DRepCertificateHistoryResponse::getCreatedAt).reversed()
                    : Comparator.comparing(DRepCertificateHistoryResponse::getCreatedAt))
            .toList();

    return new BaseFilterResponse<>(
        BaseFilterResponse.getPageImpl(dRepCertificateHistoryResponses, pageable));
  }

  @Override
  public VotingProcedureChartResponse getVoteProcedureChart(
      String dRepHashOrId, GovActionType govActionType) {
    List<VotingProcedureProjection> votingProcedureProjectionListResponse;
    Map<Vote, Long> counted;

    DRepInfo dRepInfo =
        drepInfoRepository
            .findByDRepHashOrDRepId(dRepHashOrId)
            .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));

    // if dRepHashOrId is a DRep id
    dRepHashOrId = dRepInfo.getDrepHash();
    List<VotingProcedureProjection> votingProcedureProjections =
        votingProcedureRepository.findVotingProcedureByVoterHashAndGovActionType(
            dRepHashOrId,
            govActionType.equals(GovActionType.ALL) ? null : govActionType,
            dRepInfo.getCreatedAt());
    votingProcedureProjectionListResponse =
        votingProcedureProjections.stream()
            .collect(
                Collectors.toMap(
                    e -> Pair.of(e.getGovActionTxHash(), e.getGovActionIndex()),
                    Function.identity(),
                    BinaryOperator.maxBy(
                        Comparator.comparing(VotingProcedureProjection::getBlockTime))))
            .values()
            .stream()
            .toList();
    counted =
        votingProcedureProjectionListResponse.stream()
            .collect(
                Collectors.groupingBy(VotingProcedureProjection::getVote, Collectors.counting()));

    return VotingProcedureChartResponse.builder()
        .dRepHash(dRepHashOrId)
        .govActionType(govActionType)
        .numberOfYesVote(counted.getOrDefault(Vote.YES, 0L))
        .numberOfNoVotes(counted.getOrDefault(Vote.NO, 0L))
        .numberOfAbstainVotes(counted.getOrDefault(Vote.ABSTAIN, 0L))
        .build();
  }

  @Override
  public DRepDetailsResponse getDRepDetails(String dRepHashOrDRepId) {
    DRepInfo dRepInfo =
        drepInfoRepository
            .findByDRepHashOrDRepId(dRepHashOrDRepId)
            .orElseThrow(() -> new BusinessException(BusinessCode.DREP_NOT_FOUND));
    DRepDetailsResponse response = dRepMapper.fromDrepInfo(dRepInfo);
    Long createdAtOfDRep = dRepInfo.getCreatedAt();
    Long count =
        latestVotingProcedureRepository.countVoteByVoterHash(
            dRepInfo.getDrepHash(), createdAtOfDRep);
    Long totalGovActionAllowedToVote =
        governanceActionRepository.countGovActionThatAllowedToVoteByBlockTimeGreaterThan(
            createdAtOfDRep);
    response.setVotingParticipation(
        totalGovActionAllowedToVote == 0
            ? null
            : (float) (count * 1.0 / totalGovActionAllowedToVote));
    return response;
  }

  @Override
  public DRepOverviewResponse getDRepOverview() {
    EpochSummary epochSummary = epochService.getCurrentEpochSummary();

    Map<DRepStatus, DRepStatusCountProjection> dRepStatusCountMap =
        drepInfoRepository.getDRepStatusCount().stream()
            .collect(Collectors.toMap(DRepStatusCountProjection::getStatus, Function.identity()));

    long countDownTime =
        Timestamp.valueOf(epochSummary.getEndTime()).getTime()
            - Timestamp.valueOf(LocalDateTime.now(ZoneOffset.UTC)).getTime();

    Long totalDReps =
        dRepStatusCountMap.values().stream().mapToLong(DRepStatusCountProjection::getCnt).sum();
    Long activeDReps =
        dRepStatusCountMap.containsKey(DRepStatus.ACTIVE)
            ? dRepStatusCountMap.get(DRepStatus.ACTIVE).getCnt()
            : 0L;
    Long inactiveDReps =
        dRepStatusCountMap.containsKey(DRepStatus.INACTIVE)
            ? dRepStatusCountMap.get(DRepStatus.INACTIVE).getCnt()
            : 0L;
    Long retiredDReps =
        dRepStatusCountMap.containsKey(DRepStatus.RETIRED)
            ? dRepStatusCountMap.get(DRepStatus.RETIRED).getCnt()
            : 0L;

    // TODO: implement abstainDReps and noConfidenceDReps and registeredDReps
    Long abstainDReps = null;
    Long noConfidenceDReps = null;
    Long registeredDReps = null;

    // TODO: implement activeStake
    BigInteger activeStake = null;
    Long delegators = drepInfoRepository.getDelegateCount();

    return DRepOverviewResponse.builder()
        .epochNo(epochSummary.getNo())
        .countDownEndTime(countDownTime)
        .epochSlotNo(epochSummary.getSlot())
        .activeStake(activeStake)
        .delegators(delegators)
        .totalDReps(totalDReps)
        .activeDReps(activeDReps)
        .inactiveDReps(inactiveDReps)
        .retiredDReps(retiredDReps)
        .abstainDReps(abstainDReps)
        .noConfidenceDReps(noConfidenceDReps)
        .registeredDReps(registeredDReps)
        .build();
  }

  @Override
  public BaseFilterResponse<DRepFilterResponse> getDRepsByFilter(
      DRepFilterRequest dRepFilterRequest, Pageable pageable) {

    long fromDate = Timestamp.valueOf(MIN_TIME).getTime() / 1000;
    fromDate = fromDate < 0 ? 0 : fromDate;
    long toDate = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);

    if (Objects.nonNull(dRepFilterRequest.getFromDate())) {
      fromDate = dRepFilterRequest.getFromDate().toEpochSecond(ZoneOffset.UTC);
    }
    if (Objects.nonNull(dRepFilterRequest.getToDate())) {
      long to = dRepFilterRequest.getToDate().toEpochSecond(ZoneOffset.UTC);
      toDate = Math.min(to, toDate);
    }

    if (dRepFilterRequest.getActiveStakeFrom() == null) {
      dRepFilterRequest.setActiveStakeFrom(BigInteger.ZERO);
    }

    if (dRepFilterRequest.getActiveStakeTo() == null) {
      dRepFilterRequest.setActiveStakeTo(BigInteger.valueOf(Long.MAX_VALUE));
    }

    if (dRepFilterRequest.getVotingPowerFrom() == null) {
      dRepFilterRequest.setVotingPowerFrom(0.0);
    }

    if (dRepFilterRequest.getVotingPowerTo() == null) {
      dRepFilterRequest.setVotingPowerTo(1.0);
    }

    Page<DRepFilterResponse> dRepInfoPage =
        drepInfoRepository
            .getDRepInfoByFilterRequest(
                dRepFilterRequest.getDrepIdOrHash(),
                dRepFilterRequest.getAnchorText(),
                dRepFilterRequest.getActiveStakeFrom(),
                dRepFilterRequest.getActiveStakeTo(),
                dRepFilterRequest.getVotingPowerFrom(),
                dRepFilterRequest.getVotingPowerTo(),
                dRepFilterRequest.getDrepStatus(),
                fromDate,
                toDate,
                pageable)
            .map(dRepMapper::fromDRepInfo);

    return new BaseFilterResponse<>(dRepInfoPage);
  }

  @Override
  public DRepRangeValuesResponse getDRepRangeValues() {
    DRepRangeProjection projection = drepInfoRepository.getDRepRangeValues();
    return dRepMapper.fromDRepRangeProjection(projection);
  }

  @Override
  public BaseFilterResponse<DRepDelegatorsResponse> getDRepDelegators(
      String drepHashOrDrepId, Pageable pageable) {
    Sort sort = pageable.getSort();
    for (Sort.Order order : sort) {
      if (order.getProperty().equals("createdAt")) {
        pageable =
            PageRequest.of(
                pageable.getPageNumber(),
                pageable.getPageSize(),
                Sort.by(order.getDirection(), "t.id"));
        break;
      }
    }

    BaseFilterResponse<DRepDelegatorsResponse> delegatorResponse = new BaseFilterResponse<>();
    delegatorResponse.setData(List.of());

    Page<DRepDelegatorProjection> dRepDelegatorProjections =
        delegationVoteRepository.getDelegationVoteByDRepHashOrDRepId(drepHashOrDrepId, pageable);

    List<DRepDelegatorsResponse> dRepDelegatorsResponseList =
        dRepDelegatorProjections.stream().map(dRepMapper::fromDRepDelegatorProjection).toList();
    // return when no data found
    if (dRepDelegatorProjections.isEmpty()) {
      return delegatorResponse;
    }
    // TODO
    // for mainnet using Koios
    if (fetchRewardDataService.useKoios()) {
      return delegatorResponse;
    }
    delegatorResponse.setTotalItems(dRepDelegatorProjections.getTotalElements());
    delegatorResponse.setData(dRepDelegatorsResponseList);
    delegatorResponse.setTotalPages(dRepDelegatorProjections.getTotalPages());
    delegatorResponse.setCurrentPage(pageable.getPageNumber());
    return delegatorResponse;
  }
}
