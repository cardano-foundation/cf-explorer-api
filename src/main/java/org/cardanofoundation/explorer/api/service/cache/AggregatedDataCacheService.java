package org.cardanofoundation.explorer.api.service.cache;

import java.time.LocalDateTime;

public interface AggregatedDataCacheService {
  LocalDateTime getLatestBlockTime();
  int getTokenCount();
}
