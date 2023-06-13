package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.TxService;
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
@RequestMapping("/api/v1/blocks")
@RequiredArgsConstructor
@Validated
public class BlockController {

  private final BlockService blockService;
  private final TxService txService;

  @GetMapping("/{blockId}")
  @LogMessage
  @Operation(summary = "Get a block detail")
  public ResponseEntity<BlockResponse> getBlockDetailByBlockId(
      @PathVariable @Parameter(description = "Block number or block hash") String blockId) {
    return ResponseEntity.ok(blockService.getBlockDetailByBlockId(blockId));
  }

  @GetMapping
  @LogMessage
  @Operation(summary = "Get all block")
  public ResponseEntity<BaseFilterResponse<BlockFilterResponse>> getAll(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
              "id"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(blockService.filterBlock(pagination.toPageable()));
  }

  @GetMapping("/{blockId}/txs")
  @LogMessage
  @Operation(summary = "Get tx list of block")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactionsByBlock(
      @PathVariable @Parameter(description = "Block number or block hash") String blockId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
              "blockId", "blockIndex"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByBlock(blockId, pagination.toPageable()));
  }

}
