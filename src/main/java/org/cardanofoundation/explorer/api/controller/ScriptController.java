package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.config.LogMessage;
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
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.Script_;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/scripts")
@RequiredArgsConstructor
@Validated
@Tag(name = "script", description = "The script APIs")
public class ScriptController {

  private final ScriptService scriptService;

  @GetMapping("/native-scripts")
  public ResponseEntity<BaseFilterResponse<NativeScriptFilterResponse>> getNativeScripts(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Script_.TX},
          direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getNativeScripts(pagination.toPageable()));
  }

  @GetMapping("/native-scripts/{scriptHash}")
  public ResponseEntity<NativeScriptResponse> getNativeScriptDetail(
      @PathVariable String scriptHash) {
    return ResponseEntity.ok(scriptService.getNativeScriptDetail(scriptHash));
  }

  @PostMapping("/native-scripts/{scriptHash}/verify")
  @LogMessage
  @Operation(summary = "Verify native scrip contract")
  public ResponseEntity<String> verifyContract(
      @PathVariable String scriptHash,
      @RequestBody String jsonScript) {
    return ResponseEntity.ok(scriptService.verifyNativeScript(scriptHash, jsonScript));
  }

  @GetMapping("/native-scripts/{scriptHash}/tokens")
  @LogMessage
  @Operation(summary = "Get tokens of a policy", description = "Get all tokens of a policy")
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> getTokens(
      @PathVariable @Parameter(description = "The native script hash") String scriptHash,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getNativeScriptTokens(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/native-scripts/{scriptHash}/holders")
  @LogMessage
  @Operation(summary = "Get holders by policy", description = "Get all holders of all tokens of policy")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getHolders(
      @PathVariable @Parameter(description = "The native script hash") String scriptHash,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getNativeScriptHolders(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/contracts")
  public ResponseEntity<BaseFilterResponse<SmartContractFilterResponse>> getSmartContracts(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Script_.TX},
          direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getSmartContracts(pagination.toPageable()));
  }

  @GetMapping("/contracts/{scriptHash}")
  @LogMessage
  @Operation(summary = "Get smart contract detail", tags = {"script"})
  public ResponseEntity<SmartContractDetailResponse> getSmartContracts(
      @PathVariable @Parameter(description = "The script hash")  String scriptHash) {
    return ResponseEntity.ok(scriptService.getSmartContractDetail(scriptHash));
  }

  @GetMapping("/contracts/{scriptHash}/txs")
  @LogMessage
  @Operation(summary = "Get transactions interact with smart contract", tags = {"script"})
  public ResponseEntity<BaseFilterResponse<SmartContractTxResponse>> getSmartContractsTxs(
      @PathVariable @Parameter(description = "The script hash") String scriptHash,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(
        scriptService.getSmartContractTxs(scriptHash, pagination.toPageable()));
  }

  @GetMapping("/search/{scriptHash}")
  @LogMessage
  @Operation(summary = "Search script by script hash", tags = {"script"})
  public ResponseEntity<ScriptSearchResponse> getScriptByHash(
      @PathVariable @Parameter(description = "The script hash")  String scriptHash) {
    return ResponseEntity.ok(scriptService.searchScript(scriptHash));
  }
}