package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.service.DelegationService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pool")
@RequiredArgsConstructor
public class PoolController {

  private final DelegationService delegationService;

  @GetMapping("/registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size) {
    return delegationService.getDataForPoolRegistration(page, size);
  }

  @GetMapping("/de-registration")
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size) {
    return delegationService.getDataForPoolDeRegistration(page, size);
  }
}
