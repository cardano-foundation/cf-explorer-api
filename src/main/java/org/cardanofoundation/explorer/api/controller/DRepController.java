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
import org.cardanofoundation.explorer.api.model.request.drep.DRepFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepOverviewResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.service.DRepService;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/dreps")
@RequiredArgsConstructor
@Validated
@Tag(name = "dRep", description = "The delegated representatives APIs")
public class DRepController {

  private final DRepService dRepService;

  @GetMapping("/{drepHashOrDrepId}/certificates-history")
  @LogMessage
  @Operation(
      summary = "Get list of DRep certificate history",
      tags = {"dRep"})
  public ResponseEntity<BaseFilterResponse<DRepCertificateHistoryResponse>>
      getTxDRepCertificatesHistory(
          @PathVariable @Parameter(description = "The DRep id or DRep hash")
              String drepHashOrDrepId,
          @ParameterObject
              @PaginationValid
              @PaginationDefault(
                  size = 20,
                  sort = {"createdAt"},
                  direction = Sort.Direction.DESC)
              Pagination pagination) {
    return ResponseEntity.ok(
        dRepService.getTxDRepCertificateHistory(drepHashOrDrepId, pagination.toPageable()));
  }

  @GetMapping("/{dRepHashOrId}/vote-procedure-chart")
  @LogMessage
  @Operation(
      summary = "Get chart of DRep vote on Governance Action",
      tags = {"dRep"})
  public ResponseEntity<VotingProcedureChartResponse> getChartOfDRepVotesOnGovernanceAction(
      @PathVariable @Parameter(description = "The DRep hash or id") String dRepHashOrId,
      @RequestParam(value = "govActionType")
          @Parameter(description = "The type of Governance Action")
          GovActionType govActionType) {
    return ResponseEntity.ok(dRepService.getVoteProcedureChart(dRepHashOrId, govActionType));
  }

  @GetMapping("/{dRepHashOrDRepId}/drep-details")
  @LogMessage
  @Operation(
      summary = "Get details of Delegated Representative (DRep)",
      tags = {"dRep"})
  public ResponseEntity<DRepDetailsResponse> getDRepDetails(
      @Valid @PathVariable @Parameter(description = "The DRep id or DRep hash")
          String dRepHashOrDRepId) {
    return ResponseEntity.ok(dRepService.getDRepDetails(dRepHashOrDRepId));
  }

  @GetMapping("/{dRepHashOrDRepId}/get-delegation")
  @LogMessage
  @Operation(
      summary = "Get stake that delegated to Delegated Representative (DRep)",
      tags = {"dRep"})
  public ResponseEntity<BaseFilterResponse<DRepDelegatorsResponse>> getDRepDelegation(
      @Valid @PathVariable @Parameter(description = "dRepHashOrDRepId") String dRepHashOrDRepId,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        dRepService.getDRepDelegators(dRepHashOrDRepId, pagination.toPageable()));
  }

  @GetMapping("/overview")
  @LogMessage
  @Operation(
      summary = "Get overview of Delegated Representatives (DRep)",
      tags = {"dRep"})
  public ResponseEntity<DRepOverviewResponse> getDRepOverview() {
    return ResponseEntity.ok(dRepService.getDRepOverview());
  }

  @GetMapping("/filter")
  @LogMessage
  @Operation(
      summary = "Get list of DRep by filter",
      tags = {"dRep"})
  public ResponseEntity<BaseFilterResponse<DRepFilterResponse>> getDRepsByFilter(
      @ParameterObject @Valid DRepFilterRequest dRepFilterRequest,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {"createdAt"},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {

    return ResponseEntity.ok(
        dRepService.getDRepsByFilter(dRepFilterRequest, pagination.toPageable()));
  }

  @GetMapping("/range-values-for-filter")
  @LogMessage
  @Operation(
      summary = "Get range value to filter on DRep overview page",
      tags = {"dRep"})
  public ResponseEntity<DRepRangeValuesResponse> getDRepRangeValues() {
    return ResponseEntity.ok(dRepService.getDRepRangeValues());
  }
}
