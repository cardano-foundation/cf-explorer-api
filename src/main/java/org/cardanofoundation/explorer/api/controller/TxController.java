package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.service.TxService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;

import java.util.List;

import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.common.validation.length.LengthValid;
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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/txs")
@RequiredArgsConstructor
@Validated
@Tag(name = "transaction", description = "The transaction APIs")
public class TxController {

  private final TxService txService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter transaction")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> filter(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "blockId", "blockIndex"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(txService.getAll(pagination.toPageable()));
  }

  @GetMapping("/{hash}")
  @LogMessage
  @Operation(summary = "Get transaction detail by hash")
  public ResponseEntity<TxResponse> getTransactionDetail(
      @PathVariable @Parameter(description = "The hash identifier of the transaction.")
      @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(txService.getTxDetailByHash(hash));
  }

  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current transactions")
  public ResponseEntity<List<TxSummary>> findCurrentTransaction() {
    return ResponseEntity.ok(txService.findLatestTxSummary());
  }

  @GetMapping("/graph/{range}")
  @LogMessage
  @Operation(summary = "Get transaction chart (1D , 1W, 2W, 1M)")
  public ResponseEntity<List<TxGraph>> getTransactionChart(
      @PathVariable("range") @Parameter(description = "Type for chart") TxChartRange range) {
    return ResponseEntity.ok(txService.getTransactionChartByRange(range));
  }

  @GetMapping("/{hash}/contract")
  @LogMessage
  @Operation(summary = "Get tx contract detail by txHash and contract address(optional) ")
  public ResponseEntity<List<ContractResponse>> getTxContractDetailByTxHash(
      @PathVariable("hash") @Parameter(description = "The hash identifier of the transaction.") String hash,
      @RequestParam(required = false) @Parameter(description = "The view of contract") String address) {
    return ResponseEntity.ok(txService.getTxContractDetail(hash, address));
  }
}
