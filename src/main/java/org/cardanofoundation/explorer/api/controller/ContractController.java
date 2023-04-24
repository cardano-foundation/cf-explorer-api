package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
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
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(addressService.getContracts(pageable));
  }
}
