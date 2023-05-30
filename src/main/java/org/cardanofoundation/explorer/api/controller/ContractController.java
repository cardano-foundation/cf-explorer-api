package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.consumercommon.entity.Address_;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/contracts")
@RequiredArgsConstructor
public class ContractController {

  private final AddressService addressService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Get list contract")
  public ResponseEntity<BaseFilterResponse<ContractFilterResponse>> filterContract(
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          Address_.BALANCE}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(addressService.getContracts(pageable));
  }

  @PostMapping("/verify/native")
  @LogMessage
  @Operation(summary = "Verify native scrip contract")
  public ResponseEntity<Boolean> verifyContract(@ParameterObject ScriptVerifyRequest scriptVerifyRequest) {
    return ResponseEntity.ok(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @GetMapping("/{address}/script")
  @LogMessage
  @Operation(summary = "Verify native scrip contract")
  public ResponseEntity<String> getScriptOfContract(@PathVariable String address) {
    return ResponseEntity.ok(addressService.getJsonNativeScript(address));
  }
}
