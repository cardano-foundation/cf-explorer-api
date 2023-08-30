package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.PoolRetire_;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate_;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pools")
@RequiredArgsConstructor
@Validated
@Tag(name = "pools", description = "The pool APIs")
public class PoolController {

  private final PoolRegistrationService poolRegistrationService;

  @GetMapping("/registration")
  @LogMessage
  @Operation(summary = "Get list of pool registrations", tags = {"pools"})
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          PoolUpdate_.REGISTERED_TX_ID}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolRegistration(pagination.toPageable()));
  }

  @GetMapping("/de-registration")
  @LogMessage
  @Operation(summary = "Get list of pool de-registrations", tags = {"pools"})
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          PoolRetire_.ANNOUNCED_TX_ID}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolDeRegistration(pagination.toPageable()));
  }
}
