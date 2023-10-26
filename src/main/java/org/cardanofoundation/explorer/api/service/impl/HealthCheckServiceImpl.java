package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

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

    if(Objects.isNull(latestBlockInsertTime)){
      syncStatus.setMessage(DATA_IS_NOT_SYNCING);
      syncStatus.setIsSyncing(Boolean.FALSE);
      return syncStatus;
    }

    if (Objects.isNull(latestBlockTime)) {
      Optional<Block> latestBlock = blockRepository.findLatestBlock();
      if (latestBlock.isEmpty()) {
        syncStatus.setMessage(DATA_IS_NOT_SYNCING);
        syncStatus.setIsSyncing(Boolean.FALSE);
        return syncStatus;
      }
      latestBlockTime = latestBlock.get().getTime().toLocalDateTime();
    }
    boolean isSyncing;
    String message;

    if (isOutOfThreshold(insertedTimeThresholdInSecond, latestBlockInsertTime)) {
      isSyncing = false;
      message = DATA_IS_NOT_SYNCING;
    } else {
      isSyncing = true;
      message = SYNCING_BUT_NOT_READY;
    }

    if (isSyncing) {
      if (isOutOfThreshold(blockTimeThresholdInSecond, latestBlockTime)) {
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
