package org.cardanofoundation.explorer.api.service;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

import org.cardanofoundation.explorer.api.common.enumeration.PoolActionType;
import org.cardanofoundation.explorer.api.common.enumeration.PoolStatus;
import org.cardanofoundation.explorer.api.mapper.PoolCertificateMapperImpl;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolCertificateServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.PoolCertificateProjectionImpl;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class PoolCertificateServiceTest {

  @Mock
  PoolUpdateRepository poolUpdateRepository;
  @Mock
  PoolRetireRepository poolRetireRepository;
  @Mock
  EpochRepository epochRepository;
  PoolCertificateServiceImpl poolCertificateService;

  @BeforeEach
  void setUp() {
    poolCertificateService = new PoolCertificateServiceImpl(poolUpdateRepository,
                                                            poolRetireRepository, epochRepository,
                                                            new PoolCertificateMapperImpl());
  }

  @Test
  void getTxPoolCertificateHistory_shouldReturnTxPoolCertificateHistory() {
    PoolCertificateProjectionImpl certificateUpdate1 = PoolCertificateProjectionImpl.builder()
        .txId(1L).txEpochNo(5).certEpochNo(8).certIndex(0).poolUpdateId(1L)
        .blockTime(Timestamp.valueOf("2021-01-01 00:00:00")).build();

    PoolCertificateProjectionImpl certificateUpdate2 = PoolCertificateProjectionImpl.builder()
        .txId(2L).txEpochNo(6).certEpochNo(8).certIndex(0).poolUpdateId(2L)
        .blockTime(Timestamp.valueOf("2021-02-01 00:00:00")).build();
    PoolCertificateProjectionImpl certificateRetire1 = PoolCertificateProjectionImpl.builder()
        .txId(3L).txEpochNo(7).certEpochNo(9).certIndex(0).poolRetireId(1L)
        .blockTime(Timestamp.valueOf("2021-03-01 00:00:00")).build();
    PoolCertificateProjectionImpl certificateUpdate3 = PoolCertificateProjectionImpl.builder()
        .txId(4L).txEpochNo(11).certEpochNo(14).certIndex(0).poolUpdateId(3L)
        .blockTime(Timestamp.valueOf("2021-04-01 00:00:00")).build();

    String poolViewOrHash = "pool1akt37u42nfegyf3atv6c3c64da7dwa9grt9kjxa8zwj9qr4y8u2";
    when(poolUpdateRepository.getPoolUpdateByPoolViewOrHash(poolViewOrHash))
        .thenReturn(List.of(certificateUpdate1, certificateUpdate2, certificateUpdate3));
    when(poolRetireRepository.getPoolRetireByPoolViewOrHash(poolViewOrHash))
        .thenReturn(List.of(certificateRetire1));

    var response = poolCertificateService.getTxPoolCertificateHistory(poolViewOrHash,
                                                                      PageRequest.of(0, 10, Sort.by(
                                                                          "createdAt")));
    var data = response.getData();
    Assertions.assertEquals(4, response.getTotalItems());
    Assertions.assertEquals(PoolActionType.POOL_REGISTRATION, data.get(0).getActions().get(0));
    Assertions.assertEquals(PoolActionType.POOL_UPDATE, data.get(1).getActions().get(0));
    Assertions.assertEquals(PoolActionType.POOL_DEREGISTRATION, data.get(2).getActions().get(0));
    Assertions.assertEquals(PoolActionType.POOL_REGISTRATION, data.get(3).getActions().get(0));
  }

  @Test
  void getCurrentPoolStatus_shouldReturnPoolActive() {
    PoolCertificateProjectionImpl latestCertificateUpdate = PoolCertificateProjectionImpl.builder()
        .txId(4L).txEpochNo(5).certEpochNo(8).certIndex(0).poolUpdateId(1L)
        .blockTime(Timestamp.valueOf("2021-01-01 00:00:00")).build();

    PoolCertificateProjectionImpl latestCertificateRetire = PoolCertificateProjectionImpl.builder()
        .txId(3L).txEpochNo(7).certEpochNo(9).certIndex(0).poolRetireId(1L)
        .blockTime(Timestamp.valueOf("2021-03-01 00:00:00")).build();

    String poolViewOrHash = "pool1akt37u42nfegyf3atv6c3c64da7dwa9grt9kjxa8zwj9qr4y8u2";
    when(poolUpdateRepository.getLastPoolUpdateByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateUpdate);
    when(poolRetireRepository.getLastPoolRetireByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateRetire);

    var response = poolCertificateService.getCurrentPoolStatus(poolViewOrHash);
    Assertions.assertEquals(PoolStatus.ACTIVE, response);
  }

  @Test
  void getCurrentPoolStatus_shouldReturnPoolRetiring() {
    PoolCertificateProjectionImpl latestCertificateUpdate = PoolCertificateProjectionImpl.builder()
        .txId(2L).txEpochNo(5).certEpochNo(8).certIndex(0).poolUpdateId(1L)
        .blockTime(Timestamp.valueOf("2021-01-01 00:00:00")).build();

    PoolCertificateProjectionImpl latestCertificateRetire = PoolCertificateProjectionImpl.builder()
        .txId(3L).txEpochNo(7).certEpochNo(9).certIndex(0).poolRetireId(1L)
        .blockTime(Timestamp.valueOf("2021-03-01 00:00:00")).build();

    String poolViewOrHash = "pool1akt37u42nfegyf3atv6c3c64da7dwa9grt9kjxa8zwj9qr4y8u2";
    when(poolUpdateRepository.getLastPoolUpdateByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateUpdate);
    when(poolRetireRepository.getLastPoolRetireByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateRetire);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(8));

    var response = poolCertificateService.getCurrentPoolStatus(poolViewOrHash);
    Assertions.assertEquals(PoolStatus.RETIRING, response);
  }

  @Test
  void getCurrentPoolStatus_shouldReturnPoolRetired() {
    PoolCertificateProjectionImpl latestCertificateUpdate = PoolCertificateProjectionImpl.builder()
        .txId(2L).txEpochNo(5).certEpochNo(8).certIndex(0).poolUpdateId(1L)
        .blockTime(Timestamp.valueOf("2021-01-01 00:00:00")).build();

    PoolCertificateProjectionImpl latestCertificateRetire = PoolCertificateProjectionImpl.builder()
        .txId(3L).txEpochNo(7).certEpochNo(9).certIndex(0).poolRetireId(1L)
        .blockTime(Timestamp.valueOf("2021-03-01 00:00:00")).build();

    String poolViewOrHash = "pool1akt37u42nfegyf3atv6c3c64da7dwa9grt9kjxa8zwj9qr4y8u2";
    when(poolUpdateRepository.getLastPoolUpdateByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateUpdate);
    when(poolRetireRepository.getLastPoolRetireByPoolHash(poolViewOrHash))
        .thenReturn(latestCertificateRetire);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(10));

    var response = poolCertificateService.getCurrentPoolStatus(poolViewOrHash);
    Assertions.assertEquals(PoolStatus.RETIRED, response);
  }
}
