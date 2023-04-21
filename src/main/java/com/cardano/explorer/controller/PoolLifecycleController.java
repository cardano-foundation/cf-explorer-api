package com.cardano.explorer.controller;

import com.cardano.explorer.model.request.pool.lifecycle.PoolUpdateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardAllResponse;
import com.cardano.explorer.service.PoolLifecycleService;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
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
      @RequestBody
      PoolUpdateRequest poolUpdateRequest) {
    return ResponseEntity.ok(poolLifecycleService.poolUpdate(poolUpdateRequest, pageable));
  }

  @GetMapping(value = "/reward")
  public ResponseEntity<RewardAllResponse> reward(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return null;
  }

  @GetMapping(value = "/de-registration")
  public ResponseEntity<DeRegistrationResponse> deRegistration(
      @RequestParam("poolView") String poolView) {
    return null;
  }

  @GetMapping(value = "/owner")
  public ResponseEntity<BaseFilterResponse<String>> poolOwner(
      @RequestParam("stakeKey") String stakeKey,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolLifecycleService.getPoolViewByStakeKey(stakeKey, pageable));
  }
}
