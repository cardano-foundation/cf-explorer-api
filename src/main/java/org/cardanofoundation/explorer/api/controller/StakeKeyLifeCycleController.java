package org.cardanofoundation.explorer.api.controller;

import java.util.Date;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.*;
import org.cardanofoundation.explorer.api.service.StakeKeyLifeCycleService;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance_;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration_;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stake-lifecycle")
@RequiredArgsConstructor
@Validated
public class StakeKeyLifeCycleController {

  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;

  @GetMapping("/{stakeKey}")
  @LogMessage
  public ResponseEntity<StakeLifecycleResponse> getStakeLifeCycle(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeLifeCycle(stakeKey));
  }

  @GetMapping("/{stakeKey}/registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeRegistrations(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/de-registrations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeDeRegistrations(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/delegations")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getDelegations(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDelegations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/delegations/{hash}")
  @LogMessage
  public ResponseEntity<StakeDelegationDetailResponse> getDelegationDetail(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "tx hash") @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeDelegationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/rewards")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getRewards(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @RequestParam(value = "fromDate", required = false) @DateValid(pattern = DatePattern.YYYY_MM_DD) Date fromDate,
      @RequestParam(value = "toDate", required = false) @DateValid(pattern = DatePattern.YYYY_MM_DD) Date toDate,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        BaseEntity_.ID}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewards(stakeKey, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawals")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getWithdrawals(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @Parameter(description = "filter condition") @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        BaseEntity_.ID}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawals/{hash}")
  @LogMessage
  public ResponseEntity<StakeWithdrawalDetailResponse> getDetailWithdrawal(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "tx hash") @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/wallet-activity")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivities(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        AddressTxBalance_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWalletActivities(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/reward-activity")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeRewardActivityResponse>> getRewardActivities(
      @PathVariable @Parameter(description = "stake address view") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
        @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
        "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewardActivities(stakeKey, pagination.toPageable()));
  }

}
