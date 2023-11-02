package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import java.util.List;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
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
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/delegations")
@RequiredArgsConstructor
@Validated
@Tag(name = "delegation", description = "The delegation APIs")
public class DelegationController {

  private final DelegationService delegationService;

  @GetMapping
  @LogMessage
  @Operation(summary = "List delegations", tags = {"delegation"})
  public ResponseEntity<BaseFilterResponse<DelegationResponse>> getDelegations(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Delegation_.TX_ID},
          direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(delegationService.getDelegations(pagination.toPageable()));
  }

  @GetMapping("/header")
  @LogMessage
  @Operation(summary = "Get data for delegation header", tags = {"delegation"})
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    return ResponseEntity.ok(delegationService.getDataForDelegationHeader());
  }

  @GetMapping("/pool-list")
  @LogMessage
  @Operation(summary = "Get data for pool list", tags = {"delegation"})
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestParam(value = "search", required = false)
      @Parameter(description = "Query param for search pool by name or ticker name") String search,
      @RequestParam(value = "isShowRetired", defaultValue = "true")
      @Parameter(description = "Query param for show retired pool") boolean isShowRetired) {
    return ResponseEntity.ok(
        delegationService.getDataForPoolTable(pagination.toPageable(), search, isShowRetired));
  }

  @GetMapping("/pool-detail-header/{poolViewOrHash}")
  @LogMessage
  @Operation(summary = "Get data for pool detail header", tags = {"delegation"})
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(
      @PathVariable @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
      @Parameter(description = "The Bech32 encoding of the pool hash.")
      String poolViewOrHash) {
    return ResponseEntity.ok(delegationService.getDataForPoolDetail(poolViewOrHash));
  }

  @GetMapping("/pool-detail-analytics")
  @LogMessage
  @Operation(summary = "Get analytics for pool detail", tags = {"delegation"})
  public ResponseEntity<PoolDetailAnalyticsResponse> getAnalyticsForPoolDetail(
      @RequestParam("poolView") @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
      @Parameter(description = "The Bech32 encoding of the pool hash.")
      String poolViewOrHash) {
    return ResponseEntity.ok(delegationService.getAnalyticsForPoolDetail(poolViewOrHash));
  }

  @GetMapping("/pool-detail-epochs")
  @LogMessage
  @Operation(summary = "Get epochs for pool detail", tags = {"delegation"})
  public ResponseEntity<BaseFilterResponse<PoolDetailEpochResponse>> getEpochListForPoolDetail(
      @RequestParam("poolView") @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
      @Parameter(description = "The Bech32 encoding of the pool hash.")
      String poolViewOrHash, @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        delegationService.getEpochListForPoolDetail(pagination.toPageable(), poolViewOrHash));
  }

  @GetMapping("/pool-detail-delegators")
  @LogMessage
  @Operation(summary = "Get delegators for pool detail", tags = {"delegation"})
  public ResponseEntity<BaseFilterResponse<PoolDetailDelegatorResponse>> getDelegatorForPoolDetail(
      @RequestParam("poolView") @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
      @Parameter(description = "The Bech32 encoding of the pool hash.")
      String poolViewOrHash, @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        delegationService.getDelegatorsForPoolDetail(pagination.toPageable(), poolViewOrHash));
  }

  @GetMapping("/top")
  @LogMessage
  @Operation(summary = "Find Top(default is 3) Delegation Pool order by pool size", tags = {"delegation"})
  public ResponseEntity<List<PoolResponse>> findTopDelegationPool(
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(delegationService.findTopDelegationPool(pagination.toPageable()));
  }
}
