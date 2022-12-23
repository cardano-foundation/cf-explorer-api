package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.PolicyResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.service.PolicyService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/policy")
@RequiredArgsConstructor
public class PolicyController {

  private final PolicyService policyService;

  @GetMapping("/{policyId}")
  @LogMessage
  @Operation(summary = "Get a policy detail")
  public PolicyResponse getPolicyDetail(
      @PathVariable @Parameter(description = "Policy hash") String policyId) {
    return policyService.getPolicyDetail(policyId);
  }

  @GetMapping("/{policyId}/tokens")
  @LogMessage
  @Operation(summary = "Get tokens by policy")
  public BaseFilterResponse<TokenFilterResponse> getTokens(
      @PathVariable @Parameter(description = "Policy hash") String policyId,
      @ParameterObject Pageable pageable) {
    return policyService.getTokens(policyId, pageable);
  }

  @GetMapping("/{policyId}/holders")
  @LogMessage
  @Operation(summary = "Get holders by policy")
  public BaseFilterResponse<AddressTokenProjection> getHolders(
      @PathVariable @Parameter(description = "Policy hash") String policyId,
      @ParameterObject Pageable pageable) {
    return policyService.getHolders(policyId, pageable);
  }

}
