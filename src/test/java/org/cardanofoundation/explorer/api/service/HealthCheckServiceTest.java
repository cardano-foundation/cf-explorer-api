package org.cardanofoundation.explorer.api.service;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

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
    ReflectionTestUtils.setField(healthCheckService, "blockTimeThreshold", "240");
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

  void testGetSyncStatus_WhenLatestBlockTimeAvailableInRedisAndSyncNotOK() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(5);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(30);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(latestBlockTime);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    final SyncStatus result = healthCheckService.getSyncStatus();

    assertFalse(result.getIsSyncing());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeNotAvailableInRedisAndSyncOK() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(1);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(30);
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(null);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    when(blockRepository.findLatestBlock()).thenReturn(
        Optional.ofNullable(Block.builder().time(Timestamp.valueOf(latestBlockTime)).build()));
    final SyncStatus result = healthCheckService.getSyncStatus();

    assertTrue(result.getIsSyncing());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }

  @Test
  void testGetSyncStatus_WhenLatestBlockTimeNotAvailableInRedisAndSyncNotOK() {
    var latestBlockTime = LocalDateTime.now(ZoneOffset.UTC).minusMinutes(5);
    var latestBlockInsertTime = LocalDateTime.now(ZoneOffset.UTC).minusSeconds(30);
    
    when(aggregatedDataCacheService.getLatestBlockTime()).thenReturn(null);
    when(aggregatedDataCacheService.getLatestBlockInsertTime()).thenReturn(latestBlockInsertTime);

    when(blockRepository.findLatestBlock()).thenReturn(
        Optional.ofNullable(Block.builder().time(Timestamp.valueOf(latestBlockTime)).build()));
    final SyncStatus result = healthCheckService.getSyncStatus();

    assertFalse(result.getIsSyncing());
    assertEquals(latestBlockInsertTime, result.getLatestBlockInsertTime());
  }
}
