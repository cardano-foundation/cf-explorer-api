package org.cardanofoundation.explorer.api.service.impl;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.PoolActionType;
import org.cardanofoundation.explorer.api.common.enumeration.PoolStatus;
import org.cardanofoundation.explorer.api.mapper.PoolCertificateMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCertificateProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.service.PoolCertificateService;

@Service
@RequiredArgsConstructor
public class PoolCertificateServiceImpl implements PoolCertificateService {

  private final PoolUpdateRepository poolUpdateRepository;
  private final PoolRetireRepository poolRetireRepository;
  private final EpochRepository epochRepository;

  private final PoolCertificateMapper poolCertificateMapper;

  @Override
  public BaseFilterResponse<TxPoolCertificateHistory> getTxPoolCertificateHistory(
      String poolViewOrHash,
      Pageable pageable) {

    List<TxPoolCertificateHistory> txCertificateHistories = getAllPoolCertificateHistories(
        poolViewOrHash)
        .stream()
        .collect(Collectors.groupingBy(PoolCertificateHistory::getTxId))
        .values()
        .stream()
        .map(poolCertificateHistoryList -> {
          TxPoolCertificateHistory txPoolCertificateHistory = poolCertificateMapper
              .fromPoolCertificateHistory(poolCertificateHistoryList.get(0));
          txPoolCertificateHistory.setActions(poolCertificateHistoryList
                                                  .stream()
                                                  .map(PoolCertificateHistory::getActionType)
                                                  .toList());
          return txPoolCertificateHistory;
        })
        .collect(Collectors.toList());

    txCertificateHistories.sort(
        Sort.Direction.DESC.equals(pageable.getSort().getOrderFor("createdAt").getDirection()) ?
        Comparator.comparing(TxPoolCertificateHistory::getBlockTime).reversed() :
        Comparator.comparing(TxPoolCertificateHistory::getBlockTime));

    return new BaseFilterResponse<>(
        BaseFilterResponse.getPageImpl(txCertificateHistories, pageable));
  }

  @Override
  public PoolStatus getCurrentPoolStatus(String poolViewOrHash) {
    Integer currentEpochNo = epochRepository.findCurrentEpochNo().orElse(0);
    PoolCertificateHistory latestPoolUpdate = poolCertificateMapper.fromPoolCertificateProjection(
        poolUpdateRepository.getLastPoolUpdateByPoolHash(poolViewOrHash));

    PoolCertificateHistory latestPoolRetire = poolCertificateMapper.fromPoolCertificateProjection(
        poolRetireRepository.getLastPoolRetireByPoolHash(poolViewOrHash));

    PoolStatus poolStatus = null;
    long latestPoolUpdateTxId = latestPoolUpdate == null ? -1 : latestPoolUpdate.getTxId();
    long latestPoolRetireTxId = latestPoolRetire == null ? -1 : latestPoolRetire.getTxId();
    long latestPoolUpdateCertIndex =
        latestPoolUpdate == null ? -1 : latestPoolUpdate.getCertIndex();
    long latestPoolRetireCertIndex =
        latestPoolRetire == null ? -1 : latestPoolRetire.getCertIndex();
    if (latestPoolUpdateTxId > latestPoolRetireTxId) {
      poolStatus = PoolStatus.ACTIVE;
    } else if (latestPoolUpdateTxId < latestPoolRetireTxId ||
        (latestPoolUpdateTxId == latestPoolRetireTxId
            && latestPoolUpdateCertIndex < latestPoolRetireCertIndex)) {
      poolStatus = latestPoolRetire.getCertEpochNo() > currentEpochNo ?
                   PoolStatus.RETIRING :
                   PoolStatus.RETIRED;
    }
    return poolStatus;
  }

  @Override
  public List<PoolCertificateHistory> getPoolCertificateByAction(
      String poolViewOrHash, PoolActionType action) {
    return getAllPoolCertificateHistories(poolViewOrHash).stream()
        .filter(poolCertificate -> poolCertificate.getActionType().equals(action))
        .sorted(Comparator.comparing(PoolCertificateHistory::getTxId)).toList();
  }

  private List<PoolCertificateHistory> getAllPoolCertificateHistories(String poolViewOrHash) {
    List<PoolCertificateHistory> certificateHistories =
        Stream.concat(poolUpdateRepository.getPoolUpdatePoolViewOrHash(poolViewOrHash).stream(),
                      poolRetireRepository.getPoolRetireByPoolViewOrHash(poolViewOrHash).stream())
            .sorted(Comparator
                        .comparing(PoolCertificateProjection::getTxId)
                        .thenComparing(PoolCertificateProjection::getCertIndex))
            .map(poolCertificateMapper::fromPoolCertificateProjection)
            .toList();

    for (int i = 0; i < certificateHistories.size(); i++) {
      PoolCertificateHistory certificateHistory = certificateHistories.get(i);
      if (i == 0) {
        certificateHistory.setActionType(PoolActionType.POOL_REGISTRATION);
        continue;
      }
      if (!Objects.isNull(certificateHistory.getPoolRetireId())) {
        certificateHistory.setActionType(PoolActionType.POOL_DEREGISTRATION);
      } else if (!Objects.isNull(certificateHistory.getPoolUpdateId())) {
        PoolCertificateHistory previousCertificateHistory = certificateHistories.get(i - 1);
        if (previousCertificateHistory.getActionType().equals(PoolActionType.POOL_DEREGISTRATION) &&
            certificateHistory.getTxEpochNo() > previousCertificateHistory.getCertEpochNo()) {
          certificateHistory.setActionType(PoolActionType.POOL_REGISTRATION);
        } else {
          certificateHistory.setActionType(PoolActionType.POOL_UPDATE);
        }
      }
    }

    return certificateHistories;
  }
}