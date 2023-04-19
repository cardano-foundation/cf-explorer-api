package com.cardano.explorer.controller;

import com.cardano.explorer.model.request.pool.lifecycle.PoolUpdateRequest;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardAllResponse;
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


  @GetMapping(value = "/registration")
  public ResponseEntity<RegistrationAllResponse> registration(
      @RequestParam("poolView") String poolView) {
    return null;
  }

  @GetMapping(value = "/pool-update")
  public ResponseEntity<PoolUpdateAllResponse> poolUpdate(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable,
      @RequestBody
      PoolUpdateRequest poolUpdateRequest) {
    return null;
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
}
