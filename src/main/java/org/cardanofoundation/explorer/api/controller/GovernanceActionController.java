package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;

@RestController
@RequestMapping("/api/v1/gov-actions")
@RequiredArgsConstructor
@Validated
@Tag(name = "gov-actions", description = "The governance action APIs")
public class GovernanceActionController {

  private final GovernanceActionService governanceActionService;

  @GetMapping("{dRepHashOrPoolHash}")
  @LogMessage
  @Operation(
      summary = "Get governance action that vote by DRep or pool",
      tags = {"drep"})
  public ResponseEntity<BaseFilterResponse<GovernanceActionResponse>> getGovActionByFilter(
      @PathVariable @Parameter(description = "The DRep hash or pool hash")
          String dRepHashOrPoolHash,
      GovernanceActionFilter governanceActionFilter,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActions(
            dRepHashOrPoolHash, governanceActionFilter, pageable));
  }
}
