package org.cardanofoundation.explorer.api.controller;

import java.util.Set;

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

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.entity.explorer.NativeScriptInfo_;
import org.cardanofoundation.explorer.common.entity.ledgersync.TokenTxCount_;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/scripts")
@RequiredArgsConstructor
@Validated
@Tag(name = "script", description = "The script APIs")
public class ScriptController {

  private final ScriptService scriptService;

  @GetMapping("/native-scripts")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<NativeScriptFilterResponse>> getNativeScripts(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {NativeScriptInfo_.NUMBER_OF_ASSET_HOLDERS},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination,
      @ParameterObject @Parameter(description = "filter condition")
          NativeScriptFilterRequest filterRequest) {
    return ResponseEntity.ok(
        scriptService.getNativeScripts(filterRequest, pagination.toPageable()));
  }

  @GetMapping("/native-scripts/{scriptHash}")
  @LogMessage
  public ResponseEntity<NativeScriptResponse> getNativeScriptDetail(
      @PathVariable String scriptHash) {
    return ResponseEntity.ok(scriptService.getNativeScriptDetail(scriptHash));
  }

  @PostMapping("/native-scripts/{scriptHash}/verify")
  @LogMessage
  @Operation(summary = "Verify native scrip contract")
  public ResponseEntity<String> verifyContract(
      @PathVariable String scriptHash, @RequestBody String jsonScript) {
    return ResponseEntity.ok(scriptService.verifyNativeScript(scriptHash, jsonScript));
  }

  @GetMapping("/native-scripts/{scriptHash}/tokens")
  @LogMessage
  @Operation(summary = "Get tokens of a policy", description = "Get all tokens of a policy")
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> getTokens(
      @PathVariable @Parameter(description = "The native script hash") String scriptHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {TokenTxCount_.TX_COUNT},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getNativeScriptTokens(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/native-scripts/{scriptHash}/holders")
  @LogMessage
  @Operation(
      summary = "Get holders by policy",
      description = "Get all holders of all tokens of policy")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getHolders(
      @PathVariable @Parameter(description = "The native script hash") String scriptHash,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getNativeScriptHolders(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/contracts")
  public ResponseEntity<BaseFilterResponse<SmartContractFilterResponse>> getSmartContracts(
      @ParameterObject @Parameter(description = "filter condition")
          SmartContractFilterRequest filterRequest,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"txCount"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getSmartContracts(filterRequest, pagination.toPageable()));
  }

  @GetMapping("/contracts/{scriptHash}")
  @LogMessage
  @Operation(
      summary = "Get smart contract detail",
      tags = {"script"})
  public ResponseEntity<SmartContractDetailResponse> getSmartContracts(
      @PathVariable @Parameter(description = "The script hash") String scriptHash) {
    return ResponseEntity.ok(scriptService.getSmartContractDetail(scriptHash));
  }

  @GetMapping("/contracts/{scriptHash}/txs")
  @LogMessage
  @Operation(
      summary = "Get transactions interact with smart contract",
      tags = {"script"})
  public ResponseEntity<BaseFilterResponse<SmartContractTxResponse>> getSmartContractsTxs(
      @PathVariable @Parameter(description = "The script hash") String scriptHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"tx.id"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getSmartContractTxs(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/contract-executions/{txHash}/{scriptHash}")
  @LogMessage
  @Operation(
      summary = "Get smart contract execution detail",
      tags = {"script"})
  public ResponseEntity<Set<String>> getSmartContractExecutionDetail(
      @PathVariable @Parameter(description = "The transaction hash") String txHash,
      @PathVariable @Parameter(description = "The script hash") String scriptHash) {
    return ResponseEntity.ok(scriptService.getContractExecutions(txHash, scriptHash));
  }

  @GetMapping("/search/{scriptHash}")
  @LogMessage
  @Operation(
      summary = "Search script by script hash",
      tags = {"script"})
  public ResponseEntity<ScriptSearchResponse> getScriptByHash(
      @PathVariable @Parameter(description = "The script hash") String scriptHash) {
    return ResponseEntity.ok(scriptService.searchScript(scriptHash));
  }
}
