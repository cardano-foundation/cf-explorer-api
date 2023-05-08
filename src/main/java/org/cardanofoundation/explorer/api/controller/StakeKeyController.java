package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
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
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.math.BigInteger;
import java.util.List;

@RestController
@RequestMapping("/api/v1/stakes")
@RequiredArgsConstructor
public class StakeKeyController {

  private final StakeKeyService stakeService;

  private final TxService txService;

  @GetMapping("/registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeRegistration(
          @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyRegistration(pageable));
  }

  @GetMapping("/de-registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeDeRegistration(
          @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyDeRegistration(pageable));
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
      @PathVariable @Parameter(description = "Stake key") String stakeKey) {
    return ResponseEntity.ok(stakeService.getStake(stakeKey));
  }

  @GetMapping("/{stakeKey}/txs")
  @LogMessage
  @Operation(summary = "Get transactions of stake key")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(txService.getTransactionsByStake(stakeKey, pageable));
  }

  @GetMapping("/{stakeKey}/delegation-history")
  @LogMessage
  @Operation(summary = "Get delegation history of stake key")
  public ResponseEntity<BaseFilterResponse<StakeDelegationProjection>> getDelegationHistories(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDelegationHistories(stakeKey, pageable));
  }
  @GetMapping("/{stakeKey}/stake-history")
  @LogMessage
  @Operation(summary = "Get stake history of stake key")
  public ResponseEntity<BaseFilterResponse<StakeHistoryProjection>> getStakeHistories(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeService.getStakeHistories(stakeKey, pageable));
  }

  @GetMapping("/{stakeKey}/withdrawal-history")
  @LogMessage
  @Operation(summary = "Get withdrawal transaction of stake key")
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return stakeService.getWithdrawalHistories(stakeKey, pageable);
  }

  @GetMapping("/{stakeKey}/instantaneous-rewards")
  @LogMessage
  @Operation(summary = "Get reward transaction of stake key")
  public BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return stakeService.getInstantaneousRewards(stakeKey, pageable);
  }

  @GetMapping("/top-delegators")
  @LogMessage
  @Operation(summary = "Get top delegators")
  public BaseFilterResponse<StakeFilterResponse> getTopDelegators(
      @ParameterObject Pageable pageable) {
    return stakeService.getTopDelegators(pageable);
  }

  @GetMapping("/{stakeKey}/list-address")
  @LogMessage
  @Operation(summary = "Get all address of stake")
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getAddresses(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeService.getAddresses(stakeKey,pageable));
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
          @PathVariable String stakeKey, @PathVariable
  @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type) {
    return ResponseEntity.ok(stakeService.getStakeBalanceAnalytics(stakeKey, type));
  }

  @GetMapping("/analytics-reward/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get stake balance analytics")
  public ResponseEntity<List<StakeAnalyticRewardResponse>> getStakeRewardAnalytics(
          @PathVariable String stakeKey) {
    return ResponseEntity.ok(stakeService.getStakeRewardAnalytics(stakeKey));
  }

  @GetMapping("/min-max-balance/{stakeKey}")
  @LogMessage
  @Operation(summary = "Get the highest and lowest balance address")
  public ResponseEntity<List<BigInteger>> getAddressMinMaxBalance(@PathVariable String stakeKey) {
    return ResponseEntity.ok(stakeService.getAddressMinMaxBalance(stakeKey));
  }

}
