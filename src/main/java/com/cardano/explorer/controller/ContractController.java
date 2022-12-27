package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.contract.ContractFilterResponse;
import com.cardano.explorer.service.AddressService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
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
  public BaseFilterResponse<ContractFilterResponse> filterContract(
      @ParameterObject Pageable pageable) {
    return addressService.getContracts(pageable);
  }
}
