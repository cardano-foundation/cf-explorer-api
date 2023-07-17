package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;
import org.cardanofoundation.explorer.api.service.SearchService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/api/v1/search")
@RequiredArgsConstructor
public class SearchController {

  private final SearchService searchService;

  @GetMapping
  @LogMessage
  public SearchResponse search(@RequestParam String query) {
    return searchService.search(query);
  }
}
