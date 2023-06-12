package org.cardanofoundation.explorer.api.controller;

import java.util.Date;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validate.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.*;
import org.cardanofoundation.explorer.api.service.StakeKeyLifeCycleService;
import org.cardanofoundation.explorer.common.validate.length.LengthValid;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance_;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration_;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stake-lifecycle")
@RequiredArgsConstructor
public class StakeKeyLifeCycleController {

  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;

  @GetMapping("/{stakeKey}")
  @LogMessage
  public ResponseEntity<StakeLifecycleResponse> getStakeLifeCycle(
      @PathVariable @Parameter(description = "Stake key") @StakeKeyLengthValid String stakeKey) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeLifeCycle(stakeKey));
  }

  @GetMapping("/{stakeKey}/registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeRegistrations(
      @PathVariable @Parameter(description = "Stake key") @StakeKeyLengthValid String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/de-registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeDeRegistrations(
      @PathVariable @Parameter(description = "Stake key") @StakeKeyLengthValid String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/delegations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getDelegations(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @ParameterObject StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          Delegation_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDelegations(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/delegations/{hash}")
  @LogMessage
  public ResponseEntity<StakeDelegationDetailResponse> getDelegationDetail(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "tx hash") @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeDelegationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/rewards")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getRewards(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @RequestParam(value = "fromDate", required = false) Date fromDate,
      @RequestParam(value = "toDate", required = false) Date toDate,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewards(stakeKey, fromDate, toDate, pageable));
  }

  @GetMapping("/{stakeKey}/withdrawals")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getWithdrawals(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @ParameterObject @Parameter(description = "filter condition") StakeLifeCycleFilterRequest condition,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, condition, pageable));
  }

  @GetMapping("/{stakeKey}/withdrawals/{hash}")
  @LogMessage
  public ResponseEntity<StakeWithdrawalDetailResponse> getDetailWithdrawal(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "tx hash") @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/wallet-activity")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivities(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          AddressTxBalance_.TX}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWalletActivities(stakeKey, pageable));
  }

  @GetMapping("/{stakeKey}/reward-activity")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRewardActivityResponse>> getRewardActivities(
      @PathVariable @Parameter(description = "stake address view") @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewardActivities(stakeKey, pageable));
  }

}
