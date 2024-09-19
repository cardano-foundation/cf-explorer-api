package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.SearchStakingLifecycle;
import org.cardanofoundation.explorer.api.service.SearchService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

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
  public SearchResponse search(
      @RequestParam @Parameter(description = "Query param for search") String query) {
    return searchService.search(query);
  }

  @GetMapping("staking-lifecycle")
  @LogMessage
  @Operation(
      summary = "Search for staking lifecycle",
      description = "Search like for regular pool ids, pool hash or addresses (grab by stake key)",
      tags = {"search"})
  public SearchStakingLifecycle searchForStakingLifecycle(
      @RequestParam @Parameter(description = "Query param for search") String query,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20) @Valid
          Pagination pagination) {
    return searchService.searchForStakingLifecycle(query, pagination.toPageable());
  }
}
