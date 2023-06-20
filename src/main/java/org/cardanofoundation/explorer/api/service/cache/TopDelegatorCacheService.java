package org.cardanofoundation.explorer.api.service.cache;

import java.util.List;

public interface TopDelegatorCacheService {

  /**
   * Get stake id for top-balance from cache
   *
   * @return list stake id;
   */
  List<Long> getTopStakeDelegatorCache();
}
