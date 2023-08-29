package org.cardanofoundation.explorer.api.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.service.HealthCheckService;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.consumercommon.entity.Block;

@Service
@RequiredArgsConstructor
public class HealthCheckServiceImpl implements HealthCheckService {
  private final AggregatedDataCacheService aggregatedDataCacheService;
  private final BlockRepository blockRepository;

  @Value("${application.healthcheck.block-time-threshold}")
  private String blockTimeThreshold;

  @Override
  public boolean isSyncOK() {
    LocalDateTime latestBlockTime = aggregatedDataCacheService.getLatestBlockTime();
    if (Objects.isNull(latestBlockTime)) {
      Optional<Block> latestBlock = blockRepository.findLatestBlock();
      if (latestBlock.isEmpty()) {
        return false;
      }
      latestBlockTime = latestBlock.get().getTime().toLocalDateTime();
    }
    long timeDifference = ChronoUnit.SECONDS.between(latestBlockTime, LocalDateTime.now(ZoneOffset.UTC));
    long thresholdInSeconds = Long.parseLong(blockTimeThreshold);

    return timeDifference <= thresholdInSeconds;
  }
}
