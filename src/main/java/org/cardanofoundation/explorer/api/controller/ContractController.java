package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.service.AddressService;

@RestController
@RequestMapping("/api/v1/contracts")
@RequiredArgsConstructor
@Validated
@Tag(name = "contract", description = "The contract APIs")
public class ContractController {

  private final AddressService addressService;

  //  @GetMapping
  //  @LogMessage
  //  @Operation(
  //      summary = "Get summary information of all contract",
  //      tags = {"contract"})
  //  public ResponseEntity<BaseFilterResponse<ContractFilterResponse>> getAll(
  //      @ParameterObject
  //          @PaginationValid
  //          @PaginationDefault(
  //              size = 20,
  //              sort = {Address_.BALANCE},
  //              direction = Sort.Direction.DESC)
  //          @Valid
  //          Pagination pagination) {
  //    return ResponseEntity.ok(addressService.getContracts(pagination.toPageable()));
  //  }
}
