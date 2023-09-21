package org.cardanofoundation.explorer.api.service.cache;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.springframework.data.domain.Pageable;

import java.util.Optional;

public interface TokenPageCacheService {

  /**
   * get token filter response from cache scheduler
   *
   * @return page token response;
   */
  Optional<BaseFilterResponse<TokenFilterResponse>> getTokePageCache(Pageable pageable);
}
