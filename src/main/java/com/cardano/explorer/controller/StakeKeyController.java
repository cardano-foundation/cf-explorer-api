package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeTreasuryProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.cardano.explorer.service.StakeKeyService;
import io.swagger.v3.oas.annotations.Operation;
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
@RequestMapping("/api/v1/stake")
@RequiredArgsConstructor
public class StakeKeyController {

  private final StakeKeyService stakeService;

  @GetMapping("/registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeRegistration(
          @PageableDefault(sort = {"id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeService.getDataForStakeKeyRegistration(pageable));
  }

  @GetMapping("/de-registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<StakeTxResponse>> getDataForStakeDeRegistration(
          @PageableDefault(sort = {"id"}, direction = Sort.Direction.DESC) Pageable pageable) {
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
  @Operation(summary = "Get stake history of stake key")
  public BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(
      @PathVariable @Parameter(description = "Stake key") String stakeKey,
      @ParameterObject Pageable pageable) {
    return stakeService.getWithdrawalHistories(stakeKey, pageable);
  }

  @GetMapping("/{stakeKey}/instantaneous-rewards")
  @LogMessage
  @Operation(summary = "Get stake history of stake key")
  public BaseFilterResponse<StakeTreasuryProjection> getInstantaneousRewards(
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

}
