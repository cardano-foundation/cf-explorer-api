package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.Address_;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/contracts")
@RequiredArgsConstructor
@Validated
public class ContractController {

  private final AddressService addressService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Get list contract")
  public ResponseEntity<BaseFilterResponse<ContractFilterResponse>> filterContract(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
              Address_.BALANCE}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(addressService.getContracts(pagination.toPageable()));
  }

  @PostMapping("/verify/native")
  @LogMessage
  @Operation(summary = "Verify native scrip contract")
  public ResponseEntity<Boolean> verifyContract(@RequestBody ScriptVerifyRequest scriptVerifyRequest) {
    return ResponseEntity.ok(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @GetMapping("/{address}/script")
  @LogMessage
  @Operation(summary = "Get native script of contract")
  public ResponseEntity<String> getScriptOfContract(@PathVariable String address) {
    return ResponseEntity.ok(addressService.getJsonNativeScript(address));
  }
}
