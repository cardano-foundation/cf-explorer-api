package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.DelegationHeaderResponse;
import com.cardano.explorer.model.response.PoolDetailResponse;
import com.cardano.explorer.model.response.PoolResponse;
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
  public ResponseEntity<PoolDetailResponse> getDataForPoolDetail(@PathVariable Long poolId) {
    return delegationService.getDataForPoolDetail(poolId);
  }
}
