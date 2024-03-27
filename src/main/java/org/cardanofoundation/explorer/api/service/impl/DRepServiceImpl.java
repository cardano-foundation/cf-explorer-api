package org.cardanofoundation.explorer.api.service.impl;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.mapper.DRepMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.DRepService;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

@Service
@RequiredArgsConstructor
@Log4j2
public class DRepServiceImpl implements DRepService {

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final DRepCertificateMapper dRepCertificateMapper;
  private final VotingProcedureRepository votingProcedureRepository;
  private final DrepInfoRepository drepInfoRepository;
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
      String drepHash, GovActionType govActionType) {
    List<VotingProcedureProjection> votingProcedureProjectionListResponse;
    Map<Vote, Long> counted;
    List<VotingProcedureProjection> votingProcedureProjections =
        votingProcedureRepository.findVotingProcedureByVoterHashAndGovActionType(
            drepHash,
            govActionType.equals(GovActionType.ALL)
                ? null
                : org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType
                    .valueOf(govActionType.name()));
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
        .dRepHash(drepHash)
        .govActionType(govActionType)
        .numberOfYesVote(counted.get(Vote.YES))
        .numberOfNoVotes(counted.get(Vote.NO))
        .numberOfAbstainVotes(counted.get(Vote.ABSTAIN))
        .build();
  }

  @Override
  public DRepDetailsResponse getDRepDetails(String dRepHashOrDRepId) {
    return dRepMapper.fromDrepInfo(drepInfoRepository.findByDRepHashOrDRepId(dRepHashOrDRepId));
  }
}
