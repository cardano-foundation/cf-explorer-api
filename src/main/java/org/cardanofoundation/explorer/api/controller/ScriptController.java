package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.service.ScriptService;
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
@RequestMapping("/api/v1/scripts")
@RequiredArgsConstructor
@Validated
@Tag(name = "script", description = "The script APIs")
public class ScriptController {

  private final ScriptService scriptService;

  @GetMapping("/smart-contracts/{scriptHash}")
  @LogMessage
  @Operation(summary = "Get smart contract detail", tags = {"script"})
  public ResponseEntity<SmartContractDetailResponse> getSmartContracts(
      @PathVariable @Parameter(description = "The script hash")  String scriptHash) {
    return ResponseEntity.ok(scriptService.getSmartContractDetail(scriptHash));
  }

  @GetMapping("/smart-contracts/{scriptHash}/txs")
  @LogMessage
  @Operation(summary = "Get transactions interact with smart contract", tags = {"script"})
  public ResponseEntity<BaseFilterResponse<SmartContractTxResponse>> getSmartContractsTxs(
      @PathVariable @Parameter(description = "The script hash") String scriptHash,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getSmartContractTxs(scriptHash, pagination.toPageable()));
  }
}