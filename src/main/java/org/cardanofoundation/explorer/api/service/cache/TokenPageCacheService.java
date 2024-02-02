package org.cardanofoundation.explorer.api.service.cache;

import java.util.Optional;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;

public interface TokenPageCacheService {

  /**
   * get token filter response from cache scheduler
   *
   * @return page token response;
   */
  Optional<BaseFilterResponse<TokenFilterResponse>> getTokePageCache(Pageable pageable);
}
