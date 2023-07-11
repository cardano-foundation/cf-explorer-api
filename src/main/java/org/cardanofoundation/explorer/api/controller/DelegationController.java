package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.PageZeroValid;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/v1/delegations")
@RequiredArgsConstructor
@Validated
public class DelegationController {

  private final DelegationService delegationService;

  @GetMapping
  @LogMessage
  @Operation(summary = "List delegations")
  public ResponseEntity<BaseFilterResponse<DelegationResponse>> getDelegations(
       @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Delegation_.TX_ID},
          direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(delegationService.getDelegations(pagination.toPageable()));
  }

  @GetMapping("/header")
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    return ResponseEntity.ok(delegationService.getDataForDelegationHeader());
  }

  @GetMapping("/pool-list")
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(
       @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
       @RequestParam("search") String search) {
    return ResponseEntity.ok(delegationService.getDataForPoolTable(pagination.toPageable(), search));
  }

  @GetMapping("/pool-detail-header/{poolView}")
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(
       @PathVariable @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW) @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolView) {
    return ResponseEntity.ok(delegationService.getDataForPoolDetail(poolView));
  }

  @GetMapping("/pool-detail-analytics")
  public ResponseEntity<PoolDetailAnalyticsResponse> getAnalyticsForPoolDetail(
       @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW) @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolView) {
    return ResponseEntity.ok(delegationService.getAnalyticsForPoolDetail(poolView));
  }

  @GetMapping("/pool-detail-epochs")
  public ResponseEntity<BaseFilterResponse<PoolDetailEpochResponse>> getEpochListForPoolDetail(
       @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW) @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolView,
       @ParameterObject @PaginationValid @PaginationDefault(size = 20, page = 0) Pagination pagination) {
    return ResponseEntity.ok(delegationService.getEpochListForPoolDetail(pagination.toPageable(), poolView));
  }

  @GetMapping("/pool-detail-delegators")
  public ResponseEntity<BaseFilterResponse<PoolDetailDelegatorResponse>> getDelegatorForPoolDetail(
       @RequestParam("poolView") @PrefixedValid(CommonConstant.PREFIXED_POOL_VIEW) @LengthValid(CommonConstant.POOL_VIEW_LENGTH) String poolView,
       @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(delegationService.getDelegatorsForPoolDetail(pagination.toPageable(), poolView));
  }

  @GetMapping("/top")
  @LogMessage
  @Operation(summary = "Find Top(default is 3) Delegation Pool order by pool size")
  public ResponseEntity<List<PoolResponse>> findTopDelegationPool(@PaginationValid @PageZeroValid Pagination pagination) {
    return ResponseEntity.ok(delegationService.findTopDelegationPool(pagination.toPageable()));
  }
}