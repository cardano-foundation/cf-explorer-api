package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.SearchStakingLifecycle;

public interface SearchService {

  SearchResponse search(String query);

  SearchStakingLifecycle searchForStakingLifecycle(String query, Pageable pageable);
}
