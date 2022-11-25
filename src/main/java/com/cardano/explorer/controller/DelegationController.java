package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailListResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.service.DelegationService;
import lombok.RequiredArgsConstructor;
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

  @GetMapping("/pool-detail/{poolId}")
  public ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(@PathVariable Long poolId) {
    return delegationService.getDataForPoolDetail(poolId);
  }

  @GetMapping("/pool-detail-list")
  public ResponseEntity<PoolDetailListResponse> getListForPoolDetail(
      @RequestParam("pool") Long poolId, @RequestParam("page") Integer page,
      @RequestParam("size") Integer size) {
    return delegationService.getListForPoolDetail(page, size, poolId);
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
