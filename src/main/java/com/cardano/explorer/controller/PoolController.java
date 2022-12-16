package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.service.PoolRegistrationService;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pool")
@RequiredArgsConstructor
public class PoolController {

  private final PoolRegistrationService poolRegistrationService;

  @GetMapping("/registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolRegistration(pageable));
  }

  @GetMapping("/de-registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolDeRegistration(pageable));
  }
}
