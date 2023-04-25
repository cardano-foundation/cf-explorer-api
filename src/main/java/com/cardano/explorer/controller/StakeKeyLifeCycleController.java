package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.service.StakeKeyLifeCycleService;
import com.sotatek.cardano.common.entity.BaseEntity_;
import com.sotatek.cardano.common.entity.Delegation_;
import com.sotatek.cardano.common.entity.StakeRegistration_;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stake-lifecycle")
@RequiredArgsConstructor
public class StakeKeyLifeCycleController {

  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;

  @GetMapping("/{stakeKey}/registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeRegistrations(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/de-registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeDeRegistrations(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/delegations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getDelegations(
      @PathVariable @Parameter(description = "stake address view") String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          Delegation_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDelegations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/delegations/{hash}")
  @LogMessage
  public ResponseEntity<StakeDelegationDetailResponse> getDelegationDetail(
      @PathVariable @Parameter(description = "stake address view") String stakeKey,
      @PathVariable @Parameter(description = "tx hash") String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeDelegationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/rewards")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getRewards(
      @PathVariable @Parameter(description = "stake address view") String stakeKey,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewards(stakeKey, pageable));
  }

  @GetMapping("/{stakeKey}/withdrawals")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getWithdrawals(
      @PathVariable @Parameter(description = "stake address view") String stakeKey,
      @ParameterObject @Parameter(description = "filter condition")
      StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/withdrawals/{hash}")
  @LogMessage
  public ResponseEntity<StakeWithdrawalDetailResponse> getDetailWithdrawal(
      @PathVariable @Parameter(description = "stake address view") String stakeKey,
      @PathVariable @Parameter(description = "tx hash") String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey, hash));
  }

}
