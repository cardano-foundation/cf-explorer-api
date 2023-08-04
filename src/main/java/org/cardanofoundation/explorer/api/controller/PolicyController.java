package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.PolicyService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/policies")
@RequiredArgsConstructor
@Validated
@Tag(name = "policies", description = "The policy APIs")
public class PolicyController {

  private final PolicyService policyService;

  @GetMapping("/{policyId}")
  @LogMessage
  @Operation(summary = "Get detail information of policy", tags = {"policies"} )
  public ResponseEntity<PolicyResponse> getPolicyDetail(
      @PathVariable @Parameter(description = "The policy hash") String policyId) {
    return ResponseEntity.ok(policyService.getPolicyDetail(policyId));
  }

  @GetMapping("/{policyId}/tokens")
  @LogMessage
  @Operation(
      summary = "Get tokens of a policy",
      description = "Get all tokens of a policy",
      tags = {"policies"})
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> getTokens(
      @PathVariable @Parameter(description = "The policy hash") String policyId,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(policyService.getTokens(policyId, pagination.toPageable()));
  }

  @GetMapping("/{policyId}/holders")
  @LogMessage
  @Operation(
      summary = "Get holders by policy",
      description = "Get all holders of all tokens of policy",
      tags = {"policies"})
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getHolders(
      @PathVariable @Parameter(description = "The policy hash") String policyId,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(policyService.getHolders(policyId, pagination.toPageable()));
  }

}
