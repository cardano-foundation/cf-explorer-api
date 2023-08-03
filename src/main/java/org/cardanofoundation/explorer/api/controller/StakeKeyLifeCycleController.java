package org.cardanofoundation.explorer.api.controller;

import java.util.Date;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
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
@Tag(name = "stake-lifecycle", description = "The stake key lifecycle APIs")
public class StakeKeyLifeCycleController {

  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;

  @GetMapping("/{stakeKey}")
  @LogMessage
  @Operation(
      summary = "Check lifecycle of stake key",
      description = "Check stake key lifecycle contains: registration, de-registration, delegation, reward,  withdrawal",
      tags = {"stake-lifecycle"})
  public ResponseEntity<StakeLifecycleResponse> getStakeLifeCycle(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeLifeCycle(stakeKey));
  }

  @GetMapping("/{stakeKey}/registrations")
  @LogMessage
  @Operation(summary = "Get list registration transaction of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeRegistrationFilterResponse>> getStakeRegistrations(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/registrations/{hash}")
  @LogMessage
  @Operation(summary = "Get stake key registration transaction detail", tags = {"stake-lifecycle"})
  public ResponseEntity<StakeRegistrationDetailResponse> getStakeRegistrationDetail(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @PathVariable @Parameter(description = "The hash identifier of the transaction.")
      @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/de-registrations")
  @LogMessage
  @Operation(summary = "Get list de-registration transaction of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeRegistrationFilterResponse>> getStakeDeRegistrations(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          StakeRegistration_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/de-registrations/{hash}")
  @LogMessage
  @Operation(summary = "Get stake key de-registration transaction detail", tags = {"stake-lifecycle"})
  public ResponseEntity<StakeRegistrationDetailResponse> getStakeDeRegistrationDetail(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @PathVariable @Parameter(description = "The hash identifier of the transaction.")
      @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(
        stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/delegations")
  @LogMessage
  @Operation(summary = "Get list delegation transaction of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getDelegations(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @ParameterObject @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          Delegation_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
            stakeKeyLifeCycleService.getStakeDelegations(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/delegations/{hash}")
  @LogMessage
  @Operation(summary = "Get stake key delegation transaction detail", tags = {"stake-lifecycle"})
  public ResponseEntity<StakeDelegationDetailResponse> getDelegationDetail(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @PathVariable @Parameter(description = "The hash identifier of the transaction.") @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeDelegationDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/rewards")
  @LogMessage
  @Operation(summary = "Get list reward of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getRewards(
      @PathVariable @Parameter(description = "The Bech32 encoded version of the stake address.")
      @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @RequestParam(value = "type", required = false) RewardType type,
      @RequestParam(value = "fromDate", required = false) @DateValid(pattern = DatePattern.YYYY_MM_DD) Date fromDate,
      @RequestParam(value = "toDate", required = false) @DateValid(pattern = DatePattern.YYYY_MM_DD) Date toDate,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewards(stakeKey, fromDate, toDate, type, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawals")
  @LogMessage
  @Operation(summary = "Get list withdrawal transaction of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getWithdrawals(
      @PathVariable @Parameter(description = "The Bech32 encoded version of the stake address.")
      @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @Parameter(description = "filter condition") @DateValid StakeLifeCycleFilterRequest condition,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(
            stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, condition, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawals/{hash}")
  @LogMessage
  @Operation(summary = "Get stake key withdrawal transaction detail", tags = {"stake-lifecycle"})
  public ResponseEntity<StakeWithdrawalDetailResponse> getDetailWithdrawal(
      @PathVariable @Parameter(description = "The Bech32 encoded version of the stake address.")
      @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "The hash identifier of the transaction.")
      @LengthValid(CommonConstant.TX_HASH_LENGTH) String hash) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey, hash));
  }

  @GetMapping("/{stakeKey}/wallet-activity")
  @LogMessage
  @Operation(summary = "Get wallet activity of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivities(
      @PathVariable @Parameter(description = "The Bech32 encoded version of the stake address.")
      @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          AddressTxBalance_.TX}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeWalletActivities(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/reward-activity")
  @LogMessage
  @Operation(summary = "Get reward activity of stake key", tags = {"stake-lifecycle"})
  public ResponseEntity<BaseFilterResponse<StakeRewardActivityResponse>> getRewardActivities(
      @PathVariable @Parameter(description = "The Bech32 encoded version of the stake address.")
      @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(stakeKeyLifeCycleService.getStakeRewardActivities(stakeKey, pagination.toPageable()));
  }

}