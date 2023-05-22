package org.cardanofoundation.explorer.api.service.cache;

import java.util.List;

public interface TopDelegatorCacheService {

  /**
   * build cache of list stake id which have delegators order by balance
   */
  void buildTopStakeDelegatorCache();

  /**
   * Get stake id for top-balance from cache
   *
   * @return list stake id;
   */
  List<Long> getTopStakeDelegatorCache();
}
