package com.cardano.explorer.controller;

import com.cardano.explorer.model.request.DelegationFilterRequest;
import com.cardano.explorer.model.response.DelegationHeaderResponse;
import com.cardano.explorer.model.response.PoolDetailResponse;
import com.cardano.explorer.model.response.PoolResponse;
import com.cardano.explorer.service.DelegationService;
import javax.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
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

  @PostMapping("/pool-list")
  public ResponseEntity<Page<PoolResponse>> getDataForPoolTable(@Valid @RequestBody
  DelegationFilterRequest delegationFilterRequest) {
    return delegationService.getDataForPoolTable(delegationFilterRequest);
  }

  @GetMapping("/pool-detail/{poolId}")
  public ResponseEntity<PoolDetailResponse> getDataForPoolDetail(@PathVariable Long poolId) {
    return delegationService.getDataForPoolDetail(poolId);
  }
}
