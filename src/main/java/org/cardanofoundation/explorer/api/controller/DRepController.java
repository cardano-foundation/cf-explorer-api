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

import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.service.DRepService;
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

  @GetMapping("/{dRepHash}/vote-procedure-chart")
  @LogMessage
  @Operation(
      summary = "Get chart of DRep vote on Governance Action",
      tags = {"dRep"})
  public ResponseEntity<VotingProcedureChartResponse> getChartOfDRepVotesOnGovernanceAction(
      @PathVariable @Parameter(description = "The DRep hash") String dRepHash,
      @RequestParam(value = "govActionType")
          @Parameter(description = "The type of Governance Action")
          GovActionType govActionType) {
    return ResponseEntity.ok(dRepService.getVoteProcedureChart(dRepHash, govActionType));
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
      tags = {"drep"})
  public ResponseEntity<BaseFilterResponse<DRepDelegatorsResponse>> getDRepDelegation(
      @Valid @PathVariable @Parameter(description = "dRepHashOrDRepId") String dRepHashOrDRepId,
      @ParameterObject @PaginationValid @Valid Pagination pagination) {
    return ResponseEntity.ok(
        dRepService.getDRepDelegators(dRepHashOrDRepId, pagination.toPageable()));
  }
}
