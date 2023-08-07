package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "search", description = "The search APIs")
public class SearchController {

  private final SearchService searchService;

  @GetMapping
  @LogMessage
  @Operation(
      summary = "Search",
      description = "Search like for a block, transaction, address, epoch, pool, token, policy",
      tags = {"search"})
  public SearchResponse search(@RequestParam @Parameter(description = "Query param for search") String query) {
    return searchService.search(query);
  }
}
