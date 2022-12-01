package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;
import com.cardano.explorer.service.DelegationService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.repository.query.Param;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/delegation")
@RequiredArgsConstructor
public class DelegationController {

  private final DelegationService delegationService;

  @GetMapping("/header")
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    return delegationService.getDataForDelegationHeader();
  }

  @GetMapping("/pool-list")
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("search") String search) {
    return delegationService.getDataForPoolTable(page, size, search);
  }

  @GetMapping("/pool-detail-header/{poolId}")
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(@PathVariable Long poolId) {
    return delegationService.getDataForPoolDetail(poolId);
  }

  @GetMapping("/pool-detail-analytics")
  public ResponseEntity<PoolDetailAnalyticsResponse> getAnalyticsForPoolDetail(
      @RequestParam("pool") Long poolId) {
    return delegationService.getAnalyticsForPoolDetail(poolId);
  }

  @GetMapping("/pool-detail-epochs")
  public ResponseEntity<BaseFilterResponse<PoolDetailEpochResponse>> getEpochListForPoolDetail(
      @RequestParam("pool") Long poolId, @Param("page") Integer page, @Param("size") Integer size) {
    return delegationService.getEpochListForPoolDetail(page, size, poolId);
  }

  @GetMapping("/pool-detail-delegators")
  public ResponseEntity<BaseFilterResponse<PoolDetailDelegatorResponse>> getDelegatorForPoolDetail(
      @RequestParam("pool") Long poolId, @Param("page") Integer page, @Param("size") Integer size) {
    return delegationService.getDelegatorsForPoolDetail(page, size, poolId);
  }

  @GetMapping("/pool-registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size) {
    return delegationService.getDataForPoolRegistration(page, size);
  }

  @GetMapping("/pool-de-registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size) {
    return delegationService.getDataForPoolDeRegistration(page, size);
  }
}
