package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;

public interface SearchService {

  SearchResponse search(String query);
}
