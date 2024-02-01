package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.Address_;

@RestController
@RequestMapping("/api/v1/contracts")
@RequiredArgsConstructor
@Validated
@Tag(name = "contract", description = "The contract APIs")
public class ContractController {

  private final AddressService addressService;

  @GetMapping
  @LogMessage
  @Operation(
      summary = "Get summary information of all contract",
      tags = {"contract"})
  public ResponseEntity<BaseFilterResponse<ContractFilterResponse>> getAll(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {Address_.BALANCE},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(addressService.getContracts(pagination.toPageable()));
  }
}
