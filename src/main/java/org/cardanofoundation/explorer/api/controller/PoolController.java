package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.model.response.pool.StakePoolsChartResponse;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;
import org.cardanofoundation.explorer.api.service.PoolCertificateService;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;
import org.cardanofoundation.explorer.api.service.PoolService;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolRetire_;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolUpdate_;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/pools")
@RequiredArgsConstructor
@Validated
@Tag(name = "pools", description = "The pool APIs")
public class PoolController {

  private final PoolRegistrationService poolRegistrationService;
  private final PoolCertificateService poolCertificateService;
  private final PoolService poolService;

  @GetMapping("/registration")
  @LogMessage
  @Operation(
      summary = "Get list of pool registrations",
      tags = {"pools"})
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {PoolUpdate_.REGISTERED_TX_ID},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        poolRegistrationService.getDataForPoolRegistration(pagination.toPageable()));
  }

  @GetMapping("/de-registration")
  @LogMessage
  @Operation(
      summary = "Get list of pool de-registrations",
      tags = {"pools"})
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {PoolRetire_.ANNOUNCED_TX_ID},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        poolRegistrationService.getDataForPoolDeRegistration(pagination.toPageable()));
  }

  @GetMapping("/certificates-history/{poolViewOrHash}")
  @LogMessage
  @Operation(
      summary = "Get list of pool certificates history",
      tags = {"pools"})
  public ResponseEntity<BaseFilterResponse<TxPoolCertificateHistory>> getTxPoolCertificatesHistory(
      @PathVariable @Parameter(description = "The pool view or pool hash") String poolViewOrHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"createdAt"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        poolCertificateService.getTxPoolCertificateHistory(
            poolViewOrHash, pagination.toPageable()));
  }

  @GetMapping("/range-values-for-filter")
  @LogMessage
  @Operation(
      summary = "Get range value to filter on pool overview page",
      tags = {"pools"})
  public ResponseEntity<PoolRangeValuesResponse> getPoolRangeValues() {
    return ResponseEntity.ok(poolService.getPoolRangeValues());
  }

  @GetMapping("/stake-pools-chart")
  @LogMessage
  @Operation(
      summary = "Get number of registered and active stake pools",
      tags = {"pools"})
  public ResponseEntity<StakePoolsChartResponse> getStakepoolsChart() {
    return ResponseEntity.ok(poolService.getStakePoolsChart());
  }
}
