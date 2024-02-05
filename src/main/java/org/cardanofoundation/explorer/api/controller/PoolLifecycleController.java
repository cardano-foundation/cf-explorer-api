package org.cardanofoundation.explorer.api.controller;

import java.util.Date;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

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
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;

@RestController
@RequestMapping("/api/v1/pool-lifecycle")
@RequiredArgsConstructor
@Validated
@Tag(name = "pool-lifecycle", description = "The pool lifecycle APIs")
public class PoolLifecycleController {

  private final PoolLifecycleService poolLifecycleService;

  @GetMapping(value = "/registration")
  @LogMessage
  @Operation(
      summary = "Get pool registration list",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> registration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              sort = {"bk.time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination,
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @RequestParam(value = "txHash", required = false)
          @LengthValid(CommonConstant.TX_HASH_LENGTH)
          @Parameter(description = "The hash identifier of the transaction")
          String txHash,
      @RequestParam(value = "fromDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date fromDate,
      @RequestParam(value = "toDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.registration(
            poolViewOrHash, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/registration-detail")
  @LogMessage
  @Operation(
      summary = "Get information detail for pool registration",
      tags = {"pool-lifecycle"})
  public ResponseEntity<RegistrationResponse> registrationDetail(
      @RequestParam("poolView") @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolViewOrHash,
      @RequestParam("id") @Parameter(description = "Id of pool registration") Long id) {
    return ResponseEntity.ok(poolLifecycleService.registrationDetail(poolViewOrHash, id));
  }

  @GetMapping(value = "/pool-update")
  @LogMessage
  @Operation(
      summary = "Get pool update list",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> poolUpdate(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              sort = {"bk.time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination,
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @RequestParam(value = "txHash", required = false)
          @LengthValid(CommonConstant.TX_HASH_LENGTH)
          @Parameter(description = "The hash identifier of the transaction")
          String txHash,
      @RequestParam(value = "fromDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date fromDate,
      @RequestParam(value = "toDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.poolUpdate(
            poolViewOrHash, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-detail")
  @LogMessage
  @Operation(
      summary = "Get information detail for pool update",
      tags = {"pool-lifecycle"})
  public ResponseEntity<PoolUpdateDetailResponse> poolUpdate(
      @RequestParam("id") @Parameter(description = "Id of pool update") Long id) {
    return ResponseEntity.ok(poolLifecycleService.poolUpdateDetail(id));
  }

  @GetMapping(value = "/reward")
  @LogMessage
  @Operation(
      summary = "Get pool reward list",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<RewardResponse>> reward(
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.listReward(poolViewOrHash, pagination.toPageable()));
  }

  @GetMapping(value = "/de-registration")
  @LogMessage
  @Operation(
      summary = "Get pool de-registration list",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> deRegistration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              sort = {"bk.time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination,
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @RequestParam(value = "txHash", required = false)
          @LengthValid(CommonConstant.TX_HASH_LENGTH)
          @Parameter(description = "The hash identifier of the transaction")
          String txHash,
      @RequestParam(value = "fromDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date fromDate,
      @RequestParam(value = "toDate", required = false)
          @DateValid(pattern = DatePattern.YYYY_MM_DD)
          @Parameter(description = "Filter from date (with format: yyyy/MM/dd)")
          Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.deRegistration(
            poolViewOrHash, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/owner")
  @LogMessage
  @Operation(
      summary = "Get pool owner list",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<String>> poolOwner(
      @RequestParam("stakeKey")
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @Parameter(description = "The view of stake address owner")
          String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.getPoolViewByStakeKey(stakeKey, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-info")
  @LogMessage
  @Operation(
      summary = "Get pool information",
      tags = {"pool-lifecycle"})
  public ResponseEntity<PoolInfoResponse> poolInfo(
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash) {
    return ResponseEntity.ok(poolLifecycleService.poolInfo(poolViewOrHash));
  }

  @GetMapping(value = "/registration-list")
  @LogMessage
  @Operation(
      summary = "Get pool registration list for tabular view",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> registrationList(
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 10,
              page = 0,
              sort = {"bk.time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.registrationList(poolViewOrHash, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-list")
  @LogMessage
  @Operation(
      summary = "Get pool update list for tabular view",
      tags = {"pool-lifecycle"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> poolUpdate(
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 10,
              page = 0,
              sort = {"bk.time"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.poolUpdateList(poolViewOrHash, pagination.toPageable()));
  }

  @GetMapping(value = "/status")
  @LogMessage
  @Operation(
      summary = "Get pool status",
      tags = {"pool-lifecycle"})
  public ResponseEntity<SPOStatusResponse> poolStatus(
      @RequestParam("poolView")
          @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
          @Parameter(description = "The pool view")
          String poolViewOrHash) {
    return ResponseEntity.ok(poolLifecycleService.poolLifecycleStatus(poolViewOrHash));
  }
}
