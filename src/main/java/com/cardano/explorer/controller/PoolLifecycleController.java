package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolInfoResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.service.PoolLifecycleService;
import java.sql.Timestamp;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.query.Param;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pool-lifecycle")
@RequiredArgsConstructor
public class PoolLifecycleController {

  private final PoolLifecycleService poolLifecycleService;


  @GetMapping(value = "/registration")
  public ResponseEntity<RegistrationAllResponse> registration(
      @RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.registration(poolView));
  }

  @GetMapping(value = "/pool-update")
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> poolUpdate(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable,
      @Param("poolView") String poolView, @Param("txHash") String txHash,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.poolUpdate(poolView, txHash, fromDate, toDate, pageable));
  }

  @GetMapping(value = "/pool-update-detail")
  public ResponseEntity<PoolUpdateDetailResponse> poolUpdate(@RequestParam("id") Long id,
      @Param("previousId") Long previousId) {
    return ResponseEntity.ok(poolLifecycleService.poolUpdateDetail(id, previousId));
  }

  @GetMapping(value = "/reward")
  public ResponseEntity<BaseFilterResponse<RewardResponse>> reward(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolLifecycleService.listReward(poolView, pageable));
  }

  @GetMapping(value = "/de-registration")
  public ResponseEntity<DeRegistrationAllResponse> deRegistration(
      @RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.deRegistration(poolView));
  }

  @GetMapping(value = "/owner")
  public ResponseEntity<BaseFilterResponse<String>> poolOwner(
      @RequestParam("stakeKey") String stakeKey,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolLifecycleService.getPoolViewByStakeKey(stakeKey, pageable));
  }

  @GetMapping(value = "/pool-info")
  public ResponseEntity<PoolInfoResponse> poolInfo(@RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.poolInfo(poolView));
  }
}
