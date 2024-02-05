package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;

public interface InstantaneousRewardsService {
  BaseFilterResponse<InstantaneousRewardsResponse> getAll(Pageable pageable);
}
