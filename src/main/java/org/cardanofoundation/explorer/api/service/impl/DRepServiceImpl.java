package org.cardanofoundation.explorer.api.service.impl;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.mapper.DRepMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationVoteRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.DRepService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.DelegationVote_;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class DRepServiceImpl implements DRepService {

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final DRepCertificateMapper dRepCertificateMapper;
  private final VotingProcedureRepository votingProcedureRepository;
  private final DrepInfoRepository drepInfoRepository;
  private final DelegationVoteRepository delegationVoteRepository;
  private final FetchRewardDataService fetchRewardDataService;
  private final LatestVotingProcedureRepository latestVotingProcedureRepository;
  private final GovernanceActionRepository governanceActionRepository;
  private final DRepMapper dRepMapper;

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
        latestVotingProcedureRepository.countVoteByDRepHash(
            dRepInfo.getDrepHash(), createdAtOfDRep);
    Long totalGovActionAllowedToVote =
        governanceActionRepository.countGovActionThatAllowedToVoteByDRep(createdAtOfDRep);
    response.setVotingParticipation(
        totalGovActionAllowedToVote == 0
            ? null
            : (float) (count * 1.0 / totalGovActionAllowedToVote));
    return response;
  }

  @Override
  public BaseFilterResponse<DRepDelegatorsResponse> getDRepDelegators(
      String drepHashOrDrepId, Pageable pageable) {
    if (pageable.getSort().isUnsorted()) {
      pageable =
          PageRequest.of(
              pageable.getPageNumber(),
              pageable.getPageSize(),
              Sort.by(Direction.DESC, DelegationVote_.BLOCK_TIME));
    } else {
      Sort sort = pageable.getSort();
      for (Sort.Order order : sort) {
        if (order.getProperty().equals("createdAt")) {
          pageable =
              PageRequest.of(
                  pageable.getPageNumber(),
                  pageable.getPageSize(),
                  Sort.by(order.getDirection(), DelegationVote_.BLOCK_TIME));
          break;
        }
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
