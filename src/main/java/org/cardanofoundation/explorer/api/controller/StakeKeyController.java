package org.cardanofoundation.explorer.api.controller;

import java.util.List;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressBalance_;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxAmount_;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressRewardDistribution;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeTxResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeDeregistration_;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistration_;
import org.cardanofoundation.explorer.common.validation.pagination.PageZeroValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;

@RestController
@RequestMapping("/api/v1/stakes")
@RequiredArgsConstructor
@Validated
@Tag(name = "stake-key", description = "The Stake Key APIs")
public class StakeKeyController {

  private final StakeKeyService stakeService;

  private final TxService txService;

  @GetMapping("/registration")
  @LogMessage
  @Operation(summary = "Get stake key registration", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeRegistration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {StakeRegistration_.TX_ID},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyRegistration(pagination.toPageable()));
  }

  @GetMapping("/de-registration")
  @LogMessage
  @Operation(summary = "Get stake key de-registration", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeDeRegistration(
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {StakeDeregistration_.TX_ID},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        stakeService.getDataForStakeKeyDeRegistration(pagination.toPageable()));
  }

  @GetMapping("/address/{address}")
  @LogMessage
  @Operation(summary = "Get a stake detail by payment address", tags = "stake-key")
  public ResponseEntity<StakeAddressResponse> getStakeDetailByAddress(
      @PathVariable
          @Parameter(
              description =
                  "The human readable encoding of the output address."
                      + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.")
          String address) {
    return ResponseEntity.ok(stakeService.getStakeByAddress(address));
  }

  @GetMapping("/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get a stake detail by stake key", tags = "stake-key")
  public ResponseEntity<StakeAddressResponse> getStakeDetail(
      @PathVariable
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey) {
    return ResponseEntity.ok(stakeService.getStake(stakeKey));
  }

  @GetMapping("/{stakeKey}/txs")
  @LogMessage
  @Operation(summary = "Get transactions of stake key", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject
          @PaginationDefault(
              size = 20,
              sort = {AddressTxAmount_.BLOCK_TIME},
              direction = Sort.Direction.DESC)
          @PaginationValid
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByStake(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/delegation-history")
  @LogMessage
  @Operation(summary = "Get delegation history of stake key", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<StakeDelegationProjection>> getDelegationHistories(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        stakeService.getDelegationHistories(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/stake-history")
  @LogMessage
  @Operation(summary = "Get stake history of stake key", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<StakeHistoryProjection>> getStakeHistories(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(stakeService.getStakeHistories(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/{stakeKey}/withdrawal-history")
  @LogMessage
  @Operation(summary = "Get withdrawal transaction of stake key", tags = "stake-key")
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return stakeService.getWithdrawalHistories(stakeKey, pagination.toPageable());
  }

  @GetMapping("/{stakeKey}/instantaneous-rewards")
  @LogMessage
  @Operation(summary = "Get reward transaction of stake key", tags = "stake-key")
  public BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return stakeService.getInstantaneousRewards(stakeKey, pagination.toPageable());
  }

  @GetMapping("/top-delegators")
  @LogMessage
  @Operation(
      summary = "Get top delegators",
      description = "Get top delegators by stake amount",
      tags = "stake-key")
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(
      @ParameterObject @PaginationValid @PageZeroValid @Valid Pagination pagination) {
    return stakeService.getTopDelegators(pagination.toPageable());
  }

  @GetMapping("/{stakeKey}/list-address")
  @LogMessage
  @Operation(summary = "Get all address of stake", tags = "stake-key")
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getAddresses(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @ParameterObject
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(stakeService.getAddresses(stakeKey, pagination.toPageable()));
  }

  @GetMapping("/analytics")
  @LogMessage
  @Operation(summary = "Get active stake, live stake and total stake", tags = "stake-key")
  public ResponseEntity<StakeAnalyticResponse> getStakeAnalytics() {
    return ResponseEntity.ok(stakeService.getStakeAnalytics());
  }

  @GetMapping("/analytics-balance/{stakeKey}/{type}")
  @LogMessage
  @Operation(summary = "Get stake balance analytics", tags = "stake-key")
  public ResponseEntity<AddressChartBalanceResponse> getStakeBalanceAnalytics(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey,
      @PathVariable @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type) {
    return ResponseEntity.ok(stakeService.getStakeBalanceAnalytics(stakeKey, type));
  }

  @GetMapping("/analytics-reward/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get stake balance analytics", tags = "stake-key")
  public ResponseEntity<List<StakeAnalyticRewardResponse>> getStakeRewardAnalytics(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey) {
    return ResponseEntity.ok(stakeService.getStakeRewardAnalytics(stakeKey));
  }

  @GetMapping("/reward-distribution/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get reward distribution information", tags = "stake-key")
  public ResponseEntity<StakeAddressRewardDistribution> getStakeAddressRewardDistributionInfo(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
          @StakeKeyLengthValid
          @Parameter(description = "The Bech32 encoded version of the stake address.")
          String stakeKey) {
    return ResponseEntity.ok(stakeService.getStakeAddressRewardDistributionInfo(stakeKey));
  }
}
