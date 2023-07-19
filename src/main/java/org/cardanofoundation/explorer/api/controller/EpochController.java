package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.EpochService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/epochs")
@RequiredArgsConstructor
@Validated
public class EpochController {

  private final EpochService epochService;

  private final BlockService blockService;

  @GetMapping("/{no}")
  @LogMessage
  @Operation(summary = "Get a epoch detail by its no")
  public ResponseEntity<EpochResponse> getEpochDetail(
      @PathVariable @Parameter(description = "Epoch Number") String no) {
    return ResponseEntity.ok(epochService.getEpochDetail(no));
  }

  @GetMapping("/{no}/blocks")
  @LogMessage
  @Operation(summary = "Get block list of epoch by its no")
  public ResponseEntity<BaseFilterResponse<BlockFilterResponse>> getBlockList(
      @PathVariable @Parameter(description = "Epoch Number") String no,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {"id"},
          direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(blockService.getBlockByEpoch(no, pagination.toPageable()));
  }

  @GetMapping
  @LogMessage
  @Operation(summary = "Get all epoch")
  public ResponseEntity<BaseFilterResponse<EpochResponse>> filter(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {"id"},
          direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(epochService.getAllEpoch(pagination.toPageable()));
  }
  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current epoch")
  public ResponseEntity<EpochSummary> findCurrentEpoch(){
    return ResponseEntity.ok(epochService.getCurrentEpochSummary());
  }

}
