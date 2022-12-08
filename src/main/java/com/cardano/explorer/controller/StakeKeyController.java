package com.cardano.explorer.controller;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.service.StakeKeyService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stake")
@RequiredArgsConstructor
public class StakeKeyController {

  private final StakeKeyService stakeService;

  @GetMapping("/registration")
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeRegistration(
          @PageableDefault Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyRegistration(pageable));
  }

  @GetMapping("/de-registration")
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeDeRegistration(
          @PageableDefault Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyDeRegistration(pageable));
  }

}
