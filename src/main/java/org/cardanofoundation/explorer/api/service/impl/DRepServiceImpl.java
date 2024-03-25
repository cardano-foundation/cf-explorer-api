package org.cardanofoundation.explorer.api.service.impl;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
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

import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
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
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.DRepService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.common.entity.ledgersync.DelegationVote_;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

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
  private final EpochRepository epochRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final EpochStakeRepository epochStakeRepository;
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
  public DRepDetailsResponse getDrepDetails(String dRepHashOrDRepId) {
    return dRepMapper.fromDrepInfo(drepInfoRepository.findByDrepHashOrDrepId(dRepHashOrDRepId));
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
        delegationVoteRepository.getDelegationVoteByDrepHashOrDRepId(drepHashOrDrepId, pageable);

    List<DRepDelegatorsResponse> dRepDelegatorsResponseList =
        dRepDelegatorProjections.stream().map(dRepMapper::fromDRepDelegatorProjection).toList();
    // return when no data found
    if (dRepDelegatorProjections.isEmpty()) {
      return delegatorResponse;
    }
    // for mainnet
    //    Set<String> stakeAddresses =
    //        dRepDelegatorProjections.stream()
    //            .map(DRepDelegatorProjection::getStakeAddress)
    //            .collect(Collectors.toSet());
    //
    //    Integer currentEpoch =
    //        epochRepository
    //            .findCurrentEpochNo()
    //            .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));
    // for mainnet
    if (fetchRewardDataService.useKoios()) {
      //      List<String> stakeAddressesList = stakeAddresses.stream().toList();
      //      Boolean isStake = fetchRewardDataService.checkEpochStakeForPool(stakeAddressesList);
      //      if (Boolean.FALSE.equals(isStake)) {
      //        Boolean isFetch =
      //            stakeAddressesList.size() > 40
      //                ? null
      //                : fetchRewardDataService.fetchEpochStakeForPool(stakeAddressesList);
      //        if (Objects.isNull(isFetch)) {
      //          List<CompletableFuture<Boolean>> completableFutures = new ArrayList<>();
      //          List<List<String>> subAddressList = Lists.partition(stakeAddressesList, 25);
      //          subAddressList.forEach(
      //              addressList -> completableFutures.add(fetchEpochStakeKoios(addressList)));
      //          CompletableFuture<Void> combinedFuture =
      //              CompletableFuture.allOf(completableFutures.toArray(new CompletableFuture[0]));
      //          CompletableFuture<List<Boolean>> allResultFuture =
      //              combinedFuture.thenApply(
      //                  v -> completableFutures.stream().map(CompletableFuture::join).toList());
      //          allResultFuture
      //              .thenApply(
      //                  results -> results.stream().allMatch(result ->
      // result.equals(Boolean.TRUE)))
      //              .exceptionally(
      //                  ex -> {
      //                    log.error("Error: when fetch data from koios");
      //                    return Boolean.FALSE;
      //                  });
      //        }
      //      }
      //      List<StakeAddressProjection> addressIdList =
      //          stakeAddressRepository.getAddressIdByViewIn(stakeAddressesList);
      //
      //      Set<Long> addressIdSet =
      //
      // addressIdList.stream().map(StakeAddressProjection::getId).collect(Collectors.toSet());
      //
      //      List<StakeAddressProjection> stakeAddressProjections =
      //          epochStakeRepository.totalStakeByAddressAndPool(addressIdSet, currentEpoch);
      //      Map<String, BigInteger> stakeAddressProjectionMap =
      //          stakeAddressProjections.stream()
      //              .collect(
      //                  Collectors.toMap(
      //                      StakeAddressProjection::getView,
      // StakeAddressProjection::getTotalStake));
      //      dRepDelegatorsResponseList.forEach(
      //          delegator ->
      //
      // delegator.setTotalStake(stakeAddressProjectionMap.get(delegator.getStakeAddress())));
    }
    delegatorResponse.setTotalItems(dRepDelegatorProjections.getTotalElements());
    delegatorResponse.setData(dRepDelegatorsResponseList);
    delegatorResponse.setTotalPages(dRepDelegatorProjections.getTotalPages());
    delegatorResponse.setCurrentPage(pageable.getPageNumber());
    return delegatorResponse;
  }

  private CompletableFuture<Boolean> fetchEpochStakeKoios(List<String> addressIds) {
    return CompletableFuture.supplyAsync(
        () -> fetchRewardDataService.fetchEpochStakeForPool(addressIds));
  }
}
