package org.cardanofoundation.explorer.api.controller;

import java.util.List;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.common.enumeration.BlockPropagationChartType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.*;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/blocks")
@RequiredArgsConstructor
@Validated
@Tag(name = "block", description = "The block APIs")
public class BlockController {

  private final BlockService blockService;
  private final TxService txService;

  @GetMapping("/{blockId}")
  @LogMessage
  @Operation(
      summary = "Get detail information of block",
      tags = {"block"})
  public ResponseEntity<BlockResponse> getBlockDetailByBlockId(
      @PathVariable @Parameter(description = "The hash identifier of the block.") String blockId) {
    return ResponseEntity.ok(blockService.getBlockDetailByBlockId(blockId));
  }

  @GetMapping
  @LogMessage
  @Operation(
      summary = "Get summary information of all block",
      tags = {"block"})
  public ResponseEntity<BaseFilterResponse<BlockFilterResponse>> getAll(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"id"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(blockService.filterBlock(pagination.toPageable()));
  }

  @GetMapping("/{blockId}/txs")
  @LogMessage
  @Operation(
      summary = "Get tx list of block",
      tags = {"block"})
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactionsByBlock(
      @PathVariable @Parameter(description = "The hash identifier of the block.") String blockId,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"blockId", "blockIndex"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByBlock(blockId, pagination.toPageable()));
  }

  @GetMapping("/block-propagation")
  @LogMessage
  @Operation(
      summary = "Get block propagation",
      tags = {"block"})
  public ResponseEntity<List<BlockPropagationResponse>> getBlockPropagation(
      @Parameter(description = "The type of chart.") @RequestParam(required = false)
          BlockPropagationChartType type,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 6,
              sort = {"time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(blockService.getBlockPropagation(type, pagination.toPageable()));
  }
}
