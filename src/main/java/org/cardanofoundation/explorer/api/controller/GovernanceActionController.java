package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

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
      tags = {"gov-actions"})
  public ResponseEntity<BaseFilterResponse<GovernanceActionResponse>> getGovActionByFilter(
      @PathVariable @Parameter(description = "The DRep hash or pool hash or pool view")
          String dRepHashOrPoolHash,
      @ParameterObject GovernanceActionFilter governanceActionFilter,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"vp.blockTime"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActions(
            dRepHashOrPoolHash, governanceActionFilter, pagination.toPageable()));
  }

  @GetMapping("{dRepHashOrPoolHash}/voting-procedure-detail")
  @LogMessage
  @Operation(
      summary = "Get governance action that vote by DRep or pool",
      tags = {"gov-actions"})
  public ResponseEntity<GovernanceActionDetailsResponse> getGovActionByTxHashAndVoterHash(
      @PathVariable @Parameter(description = "The DRep hash or pool hash")
          String dRepHashOrPoolHash,
      @ParameterObject GovernanceActionRequest governanceActionRequest) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActionDetails(
            dRepHashOrPoolHash, governanceActionRequest));
  }

  @GetMapping("voting-chart")
  @LogMessage
  @Operation(
      summary = "Get voting chart of governance action",
      tags = {"gov-actions"})
  public ResponseEntity<VotingChartResponse> getVotingChartByGovAction(
      @RequestParam @Parameter(description = "The tx hash of governance action") String txHash,
      @RequestParam @Parameter(description = "The index of governance action") Integer index) {
    return ResponseEntity.ok(
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index));
  }
}
