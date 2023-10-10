package org.cardanofoundation.explorer.api.service;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Optional;

import org.springframework.test.util.ReflectionTestUtils;

import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.api.service.impl.HealthCheckServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.DATA_IS_NOT_SYNCING;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.READY_TO_SERVE;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.SYNCING_BUT_NOT_READY;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
class HealthCheckServiceTest {

  @Mock
  private AggregatedDataCacheService aggregatedDataCacheService;
  @Mock
  private BlockRepository blockRepository;
  @InjectMocks
  private HealthCheckServiceImpl healthCheckService;

  @BeforeEach
  void setup() {
    ReflectionTestUtils.setField(healthCheckService, "blockTimeThresholdInSecond", Long.valueOf(240));
    ReflectionTestUtils.setField(healthCheckService, "insertedTimeThresholdInSecond", Long.valueOf(1200));
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeAvailableInRedisAndSyncOK() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(1);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(30);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(latestBlockTime);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    final SyncStatus result = healthCheckService.getSyncStatus();

    assertTrue(result.getIsSyncing());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeAvailableInRedisAndSyncOKButNotReadyToServe() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(10);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(30);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(latestBlockTime);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    final SyncStatus result = healthCheckService.getSyncStatus();

    assertTrue(result.getIsSyncing());
    assertEquals(SYNCING_BUT_NOT_READY,result.getMessage());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeNotAvailableInRedisAndSyncOKAndReadyToSever() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(1);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(15);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(null);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    when(blockRepository.findLatestBlock()).thenReturn(
        Optional.ofNullable(Block.builder().time(Timestamp.valueOf(latestBlockTime)).build()));
    final SyncStatus result = healthCheckService.getSyncStatus();

    assertTrue(result.getIsSyncing());
    assertEquals(READY_TO_SERVE,result.getMessage());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeNotAvailableInRedisAndNotSyncing() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(6);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(1500);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(null);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    when(blockRepository.findLatestBlock()).thenReturn(
        Optional.ofNullable(Block.builder().time(Timestamp.valueOf(latestBlockTime)).build()));
    final SyncStatus result = healthCheckService.getSyncStatus();

    assertFalse(result.getIsSyncing());
    assertEquals(DATA_IS_NOT_SYNCING ,result.getMessage());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }
}
