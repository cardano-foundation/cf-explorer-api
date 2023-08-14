package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractScript;
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
@Tag(name = "contract", description = "The contract APIs")
public class ContractController {

  private final AddressService addressService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Get summary information of all contract", tags = {"contract"})
  public ResponseEntity<BaseFilterResponse<ContractFilterResponse>> getAll(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          Address_.BALANCE}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(addressService.getContracts(pagination.toPageable()));
  }

  @PostMapping("/verify/native")
  @LogMessage
  @Operation(summary = "Verify native scrip contract", tags = {"contract"})
  public ResponseEntity<Boolean> verifyContract(@RequestBody ScriptVerifyRequest scriptVerifyRequest) {
    return ResponseEntity.ok(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @GetMapping("/{address}/script")
  @LogMessage
  @Operation(
      summary = "Get native script of contract",
      description = "Check if the contract is native script contract and get the script of contract if it is native script contract.",
      tags = {"contract"})
  public ResponseEntity<ContractScript> getScriptOfContract(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address) {
    return ResponseEntity.ok(addressService.getJsonNativeScript(address));
  }
}
