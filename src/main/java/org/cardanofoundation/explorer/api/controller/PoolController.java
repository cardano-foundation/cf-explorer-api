package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validate.pagination.Pagination;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pools")
@RequiredArgsConstructor
public class PoolController {

  private final PoolRegistrationService poolRegistrationService;

  @GetMapping("/registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @ParameterObject @PaginationValid @PaginationDefault Pagination pagination) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolRegistration(pagination.toPageable()));
  }

  @GetMapping("/de-registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @ParameterObject @PaginationValid @PaginationDefault Pagination pagination) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolDeRegistration(pagination.toPageable()));
  }
}
