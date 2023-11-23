package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.HealthCheckService;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.consumercommon.entity.Block;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.DATA_IS_NOT_SYNCING;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.READY_TO_SERVE;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.SYNCING_BUT_NOT_READY;

@Service
@RequiredArgsConstructor
@Log4j2
public class HealthCheckServiceImpl implements HealthCheckService {

  private final AggregatedDataCacheService aggregatedDataCacheService;
  private final BlockRepository blockRepository;

  @Value("${application.healthcheck.block-time-threshold}")
  private Long blockTimeThresholdInSecond;

  @Value("${application.healthcheck.inserted-time-threshold}")
  private Long insertedTimeThresholdInSecond;

  @Override
  public SyncStatus getSyncStatus() {
    SyncStatus syncStatus = new SyncStatus();

    LocalDateTime latestBlockTime = aggregatedDataCacheService.getLatestBlockTime();
    LocalDateTime latestBlockInsertTime = aggregatedDataCacheService.getLatestBlockInsertTime();

    if (Objects.isNull(latestBlockInsertTime)) {
      log.warn("Latest block insert time is null");
      syncStatus.setMessage(DATA_IS_NOT_SYNCING);
      syncStatus.setIsSyncing(Boolean.FALSE);
      return syncStatus;
    }

    if (Objects.isNull(latestBlockTime)) {
      Optional<Block> latestBlock = blockRepository.findLatestBlock();
      if (latestBlock.isEmpty()) {
        log.warn("Latest block time is null, latest block is not in the database");
        syncStatus.setMessage(DATA_IS_NOT_SYNCING);
        syncStatus.setIsSyncing(Boolean.FALSE);
        return syncStatus;
      }
      latestBlockTime = latestBlock.get().getTime().toLocalDateTime();
    }
    boolean isSyncing;
    String message;

    if (isOutOfThreshold(insertedTimeThresholdInSecond, latestBlockInsertTime)) {
      log.warn("Inserted time {} over inserted time threshold {}", latestBlockInsertTime,
          insertedTimeThresholdInSecond);
      isSyncing = false;
      message = DATA_IS_NOT_SYNCING;
    } else {
      isSyncing = true;
      message = SYNCING_BUT_NOT_READY;
    }

    if (isSyncing) {
      if (isOutOfThreshold(blockTimeThresholdInSecond, latestBlockTime)) {
        log.warn("Inserted block time {} over block time threshold {}", latestBlockTime, blockTimeThresholdInSecond);
        message = SYNCING_BUT_NOT_READY;
      } else {
        message = READY_TO_SERVE;
      }
    }

    return SyncStatus.builder().isSyncing(isSyncing)
        .message(message)
        .latestBlockInsertTime(latestBlockInsertTime)
        .build();
  }

  public boolean isOutOfThreshold(Long threshold, LocalDateTime time) {
    long value = ChronoUnit.SECONDS.between(time, LocalDateTime.now(ZoneOffset.UTC));
    return threshold <= value;
  }
}
