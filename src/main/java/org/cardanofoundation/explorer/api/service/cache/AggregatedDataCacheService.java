package org.cardanofoundation.explorer.api.service.cache;

import java.time.LocalDateTime;

public interface AggregatedDataCacheService {

  /**
   * Get latest block time
   */
  LocalDateTime getLatestBlockTime();

  /**
   * Get the time when the most recent block was inserted
   */
  LocalDateTime getLatestBlockInsertTime();
}
