package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.service.DelegationService;
import io.swagger.v3.oas.annotations.Operation;
import java.math.BigInteger;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/delegations")
@RequiredArgsConstructor
public class DelegationController {

  private final DelegationService delegationService;

  @GetMapping("/header")
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    return ResponseEntity.ok(delegationService.getDataForDelegationHeader());
  }

  @GetMapping("/pool-list")
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable,
      @RequestParam("search") String search) {
    return ResponseEntity.ok(delegationService.getDataForPoolTable(pageable, search));
  }

  @GetMapping("/pool-detail-header/{poolView}")
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(
      @PathVariable String poolView) {
    return ResponseEntity.ok(delegationService.getDataForPoolDetail(poolView));
  }

  @GetMapping("/pool-detail-analytics")
  public ResponseEntity<PoolDetailAnalyticsResponse> getAnalyticsForPoolDetail(
      @RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(delegationService.getAnalyticsForPoolDetail(poolView));
  }

  @GetMapping("/pool-detail-epochs")
  public ResponseEntity<BaseFilterResponse<PoolDetailEpochResponse>> getEpochListForPoolDetail(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(delegationService.getEpochListForPoolDetail(pageable, poolView));
  }

  @GetMapping("/pool-detail-delegators")
  public ResponseEntity<BaseFilterResponse<PoolDetailDelegatorResponse>> getDelegatorForPoolDetail(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(delegationService.getDelegatorsForPoolDetail(pageable, poolView));
  }

  @GetMapping("/top")
  @LogMessage
  @Operation(summary = "Find Top(default is 3) Delegation Pool order by pool size")
  public ResponseEntity<Set<PoolResponse>> findTopDelegationPool(Pageable pageable) {
    return ResponseEntity.ok(delegationService.findTopDelegationPool(
        PageRequest.of(BigInteger.ZERO.intValue(), pageable.getPageSize())));
  }
}
