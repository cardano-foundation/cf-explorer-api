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
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.*;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
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

  @GetMapping("overview")
  @LogMessage
  @Operation(
      summary = "Get governance overview",
      tags = {"gov-actions"})
  public ResponseEntity<GovernanceOverviewResponse> getGovOverview() {
    return ResponseEntity.ok(governanceActionService.getGovernanceOverview());
  }

  @GetMapping("{voterHash}")
  @LogMessage
  @Operation(
      summary = "Get governance action that vote by voter hash",
      tags = {"gov-actions"})
  public ResponseEntity<BaseFilterResponse<GovernanceActionResponse>>
      getGovActionByVoterHashAndFilter(
          @PathVariable @Parameter(description = "The Voter Hash") String voterHash,
          @ParameterObject GovernanceActionFilter governanceActionFilter,
          @ParameterObject
              @PaginationValid
              @PaginationDefault(
                  size = 20,
                  sort = {"blockTime"},
                  direction = Sort.Direction.DESC)
              @Valid
              Pagination pagination) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActions(
            voterHash, governanceActionFilter, pagination.toPageable()));
  }

  @GetMapping
  @LogMessage
  @Operation(
      summary = "Get governance action by filter",
      tags = {"gov-actions"})
  public ResponseEntity<BaseFilterResponse<GovernanceActionResponse>> getGovActionByFilter(
      @ParameterObject GovernanceActionFilter governanceActionFilter,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"blockTime"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActions(
            null, governanceActionFilter, pagination.toPageable()));
  }

  @GetMapping("committee-history")
  @LogMessage
  @Operation(
      summary = "Get governance action committee history by filter",
      tags = {"gov-actions"})
  public ResponseEntity<BaseFilterResponse<GovernanceActionResponse>>
      getGovActionCommitteeHistoryByFilter(
          @ParameterObject GovCommitteeHistoryFilter govCommitteeHistoryFilter,
          @ParameterObject
              @PaginationValid
              @PaginationDefault(
                  size = 20,
                  sort = {"blockTime"},
                  direction = Sort.Direction.DESC)
              @Valid
              Pagination pagination) {
    return ResponseEntity.ok(
        governanceActionService.getGovCommitteeStatusHistory(
            govCommitteeHistoryFilter, pagination.toPageable()));
  }

  @GetMapping("{voterHash}/voting-procedure-detail")
  @LogMessage
  @Operation(
      summary = "Get governance action that vote by voter hash",
      tags = {"gov-actions"})
  public ResponseEntity<GovernanceActionDetailsResponse> getGovActionByTxHashAndVoterHash(
      @PathVariable @Parameter(description = "The voterHash") String voterHash,
      @ParameterObject GovernanceActionRequest governanceActionRequest) {
    return ResponseEntity.ok(
        governanceActionService.getGovernanceActionDetails(voterHash, governanceActionRequest));
  }

  @GetMapping("voting-chart")
  @LogMessage
  @Operation(
      summary = "Get voting chart of governance action",
      tags = {"gov-actions"})
  public ResponseEntity<VotingChartResponse> getVotingChartByGovAction(
      @RequestParam @Parameter(description = "The tx hash of governance action") String txHash,
      @RequestParam @Parameter(description = "The index of governance action") Integer index,
      @RequestParam @Parameter(description = "The type of voter") VoterType voterType) {
    return ResponseEntity.ok(
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index, voterType));
  }

  @GetMapping("/information")
  @LogMessage
  @Operation(
      summary = "Get governance action that vote by voter hash",
      tags = {"gov-actions"})
  public ResponseEntity<GovernanceActionOverViewResponse> getGovActionDetails(
      @RequestParam @Parameter(description = "The hash of transaction governance action")
          String txHash,
      @RequestParam @Parameter(description = "The index of transaction governance action")
          Integer index) {
    return ResponseEntity.ok(governanceActionService.getGovernanceActionInfo(txHash, index));
  }

  @GetMapping("/authors")
  @LogMessage
  @Operation(
      summary = "Get voting on governance action",
      tags = {"gov-actions"})
  public ResponseEntity<BaseFilterResponse<AuthorResponse>> getAuthorsByAnchorUrlAndAnchorHash(
      @RequestParam @Parameter(description = "The anchor url of transaction governance action")
          String anchorUrl,
      @RequestParam @Parameter(description = "The anchor hash of transaction governance action")
          String anchorHash,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 6,
              sort = {"name"},
              direction = Sort.Direction.ASC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(
        governanceActionService.getAuthorsByAnchor(anchorUrl, anchorHash, pagination.toPageable()));
  }
}
