package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;
import org.springframework.data.domain.Pageable;

public interface InstantaneousRewardsService {
  BaseFilterResponse<InstantaneousRewardsResponse> getAll(Pageable pageable);
}
