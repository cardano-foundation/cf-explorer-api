package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;
import org.cardanofoundation.explorer.api.service.InstantaneousRewardsService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/instantaneous-rewards")
@RequiredArgsConstructor
@Validated
public class InstantaneousRewardsController {

  private final InstantaneousRewardsService instantaneousRewardsService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Get list of instantaneous rewards")
  public ResponseEntity<BaseFilterResponse<InstantaneousRewardsResponse>> getInstantaneousRewards(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20) Pagination pagination) {
    return ResponseEntity.ok(instantaneousRewardsService.getAll(pagination.toPageable()));
  }
}
