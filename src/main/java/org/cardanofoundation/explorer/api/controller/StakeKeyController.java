package org.cardanofoundation.explorer.api.controller;

import java.util.concurrent.ExecutionException;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.PageZeroValid;
import org.cardanofoundation.explorer.api.controller.validation.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeTxResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.service.TxService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.math.BigInteger;
import java.util.List;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stakes")
@RequiredArgsConstructor
@Validated
public class StakeKeyController {

  private final StakeKeyService stakeService;

  private final TxService txService;

  @GetMapping("/registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeRegistration(
          @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyRegistration(pagination.toPageable()));
  }

  @GetMapping("/de-registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeDeRegistration(
          @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyDeRegistration(pagination.toPageable()));
  }
  @GetMapping("/address/{address}")
  @LogMessage
  @Operation(summary = "Get a stake detail by stake key")
  public ResponseEntity<StakeAddressResponse> getStakeDetailByAddress(
      @PathVariable @Parameter(description = "Address") String address) {
    return ResponseEntity.ok(stakeService.getStakeByAddress(address));
  }

  @GetMapping("/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get a stake detail by stake key")
  public ResponseEntity<StakeAddressResponse> getStakeDetail(
      @PathVariable @Parameter(description = "Stake key") @StakeKeyLengthValid String stakeKey) {
    return ResponseEntity.ok(stakeService.getStake(stakeKey));
  }

  @GetMapping("/{stakeKey}/txs")
  @LogMessage
  @Operation(summary = "Get transactions of stake key")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByStake(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/delegation-history")
  @LogMessage
  @Operation(summary = "Get delegation history of stake key")
  public ResponseEntity<BaseFilterResponse<StakeDelegationProjection>> getDelegationHistories(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getDelegationHistories(stakeKey, pagination.toPageable()));
  }
  @GetMapping("/{stakeKey}/stake-history")
  @LogMessage
  @Operation(summary = "Get stake history of stake key")
  public ResponseEntity<BaseFilterResponse<StakeHistoryProjection>> getStakeHistories(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getStakeHistories(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawal-history")
  @LogMessage
  @Operation(summary = "Get withdrawal transaction of stake key")
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return stakeService.getWithdrawalHistories(stakeKey, pagination.toPageable());
  }

  @GetMapping("/{stakeKey}/instantaneous-rewards")
  @LogMessage
  @Operation(summary = "Get reward transaction of stake key")
  public BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return stakeService.getInstantaneousRewards(stakeKey, pagination.toPageable());
  }

  @GetMapping("/top-delegators")
  @LogMessage
  @Operation(summary = "Get top delegators")
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(@ParameterObject @PaginationValid
                                                                    @PageZeroValid Pagination pagination) {
    return stakeService.getTopDelegators(pagination.toPageable());
  }

  @GetMapping("/{stakeKey}/list-address")
  @LogMessage
  @Operation(summary = "Get all address of stake")
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getAddresses(
      @PathVariable @Parameter(description = "Stake key") @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getAddresses(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/analytics")
  @LogMessage
  @Operation(summary = "Get active stake, live stake and total stake")
  public ResponseEntity<StakeAnalyticResponse> getStakeAnalytics() {
    return ResponseEntity.ok(stakeService.getStakeAnalytics());
  }


  @GetMapping("/analytics-balance/{stakeKey}/{type}")
  @LogMessage
  @Operation(summary = "Get stake balance analytics")
  public ResponseEntity<List<StakeAnalyticBalanceResponse>> getStakeBalanceAnalytics(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @PathVariable @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type
  ) throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(stakeService.getStakeBalanceAnalytics(stakeKey, type));
  }


  @GetMapping("/analytics-reward/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get stake balance analytics")
  public ResponseEntity<List<StakeAnalyticRewardResponse>> getStakeRewardAnalytics(
          @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey) {
    return ResponseEntity.ok(stakeService.getStakeRewardAnalytics(stakeKey));
  }

  @GetMapping("/min-max-balance/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get the highest and lowest balance address")
  public ResponseEntity<List<BigInteger>> getStakeMinMaxBalance(@PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey) {
    return ResponseEntity.ok(stakeService.getStakeMinMaxBalance(stakeKey));
  }

}
