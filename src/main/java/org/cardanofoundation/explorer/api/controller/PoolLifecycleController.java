package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolInfoResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.SPOStatusResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import java.util.Date;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.repository.query.Param;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pool-lifecycle")
@RequiredArgsConstructor
@Validated
@Tag(name = "pool-lifecycle", description = "The pool lifecycle APIs")
public class PoolLifecycleController {

  private final PoolLifecycleService poolLifecycleService;

  @GetMapping(value = "/registration")
  @LogMessage
  @Operation(summary = "Get pool registration list", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> registration(
      @ParameterObject @PaginationValid Pagination pagination,
      @Param("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @Param("txHash") @LengthValid(CommonConstant.TX_HASH_LENGTH)
      @Parameter(description = "The hash identifier of the transaction") String txHash,
      @Param("fromDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date fromDate,
      @Param("toDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date toDate) {
    return ResponseEntity.ok(
            poolLifecycleService.registration(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/registration-detail")
  @LogMessage
  @Operation(summary = "Get information detail for pool registration", tags = {"pool-lifecycle"})
  public ResponseEntity<RegistrationResponse> registrationDetail(
      @Param("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolView,
      @RequestParam("id") @Parameter(description = "Id of pool registration") Long id) {
    return ResponseEntity.ok(poolLifecycleService.registrationDetail(poolView, id));
  }

  @GetMapping(value = "/pool-update")
  @LogMessage
  @Operation(summary = "Get pool update list", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> poolUpdate(
      @ParameterObject @PaginationValid Pagination pagination,
      @Param("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @Param("txHash") @LengthValid(CommonConstant.TX_HASH_LENGTH)
      @Parameter(description = "The hash identifier of the transaction") String txHash,
      @Param("fromDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date fromDate,
      @Param("toDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date toDate) {
    return ResponseEntity.ok(
            poolLifecycleService.poolUpdate(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-detail")
  @LogMessage
  @Operation(summary = "Get information detail for pool update", tags = {"pool-lifecycle"})
  public ResponseEntity<PoolUpdateDetailResponse> poolUpdate(
      @RequestParam("id") @Parameter(description = "Id of pool update") Long id) {
    return ResponseEntity.ok(poolLifecycleService.poolUpdateDetail(id));
  }

  @GetMapping(value = "/reward")
  @LogMessage
  @Operation(summary = "Get pool reward list", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<RewardResponse>> reward(
      @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(poolLifecycleService.listReward(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/de-registration")
  @LogMessage
  @Operation(summary = "Get pool de-registration list", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> deRegistration(
      @ParameterObject @PaginationValid Pagination pagination,
      @Param("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @Param("txHash") @LengthValid(CommonConstant.TX_HASH_LENGTH)
      @Parameter(description = "The hash identifier of the transaction") String txHash,
      @Param("fromDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date fromDate,
      @Param("toDate") @DateValid(pattern = DatePattern.YYYY_MM_DD)
      @Parameter(description = "Filter from date (with format: yyyy/MM/dd)") Date toDate) {
    return ResponseEntity.ok(
            poolLifecycleService.deRegistration(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/owner")
  @LogMessage
  @Operation(summary = "Get pool owner list", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<String>> poolOwner(
      @RequestParam("stakeKey") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
      @Parameter(description = "The view of stake address owner") String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(poolLifecycleService.getPoolViewByStakeKey(stakeKey, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-info")
  @LogMessage
  @Operation(summary = "Get pool information", tags = {"pool-lifecycle"})
  public ResponseEntity<PoolInfoResponse> poolInfo(
      @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.poolInfo(poolView));
  }

  @GetMapping(value = "/registration-list")
  @LogMessage
  @Operation(summary = "Get pool registration list for tabular view", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> registrationList(
      @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(
            poolLifecycleService.registrationList(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-list")
  @LogMessage
  @Operation(summary = "Get pool update list for tabular view", tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> poolUpdate(
      @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(
            poolLifecycleService.poolUpdateList(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/status")
  @LogMessage
  @Operation(summary = "Get pool status", tags = {"pool-lifecycle"})
  public ResponseEntity<SPOStatusResponse> poolStatus(
      @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW)
      @LengthValid(CommonConstant.POOL_VIEW_LENGTH) @Parameter(description = "The pool view") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.poolLifecycleStatus(poolView));
  }
}