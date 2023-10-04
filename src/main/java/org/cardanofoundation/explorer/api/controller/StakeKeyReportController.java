package org.cardanofoundation.explorer.api.controller;

import java.util.Map;

import io.swagger.v3.oas.annotations.tags.Tag;

import jakarta.servlet.http.HttpServletRequest;

import jakarta.validation.Valid;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.ReportLimitResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.security.auth.UserPrincipal;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.service.RoleService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;


import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.springdoc.core.annotations.ParameterObject;

import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.hibernate5.SpringSessionContext;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/staking-lifecycle/report")
@RequiredArgsConstructor
@Validated
@Tag(name = "stake-key-lifecycle-report", description = "The stake key lifecycle report APIs")
public class StakeKeyReportController {

  private final StakeKeyReportService stakeKeyReportService;
  private final ReportHistoryService reportHistoryService;
  private final RoleService roleService;

  @PostMapping(value = "/stake-key")
  @LogMessage
  @Operation(summary = "Generate stake key report")
  public ResponseEntity<StakeKeyReportHistoryResponse> generateStakeKeyReport(
      @RequestBody StakeKeyReportRequest stakeKeyReportRequest,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok()
        .body(stakeKeyReportService.generateStakeKeyReport(stakeKeyReportRequest, userPrincipal));
  }

  @GetMapping(value = "/stake-key/{reportId}/export")
  @LogMessage
  @Operation(summary = "Export stake key report by id")
  public ResponseEntity<Resource> exportStakeKeyReportByStorageKey(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @RequestParam(required = false) @Parameter(description = "Type for export") ExportType exportType,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    StakeKeyReportResponse response = stakeKeyReportService.exportStakeKeyReport(reportId,
        userPrincipal.getUsername(),
        exportType);
    return ResponseEntity.ok()
        .contentLength(response.getByteArrayInputStream().available())
        .header(HttpHeaders.CONTENT_DISPOSITION,
            "attachment; filename=\"" + response.getFileName() + "\"")
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .body(new InputStreamResource(response.getByteArrayInputStream()));
  }

  @GetMapping(value = "/stake-key/{stakeKey}/history")
  @LogMessage
  @Operation(summary = "Get stake key report history by stake key")
  public ResponseEntity<BaseFilterResponse<StakeKeyReportHistoryResponse>> getStakeReportHistoryByStakeKey(
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid
      @Parameter(description = "The Bech32 encoded version of the stake address.") String stakeKey,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey,
            userPrincipal.getUsername(), pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/detail")
  @LogMessage
  @Operation(summary = "Get stake key report detail by report id")
  public ResponseEntity<StakeKeyReportHistoryResponse> getStakeReportDetail(
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByReportId(reportId,
            userPrincipal.getUsername()));
  }

  @GetMapping(value = "/stake-key/history")
  @LogMessage
  @Operation(summary = "Get all stake key report history")
  public ResponseEntity<BaseFilterResponse<StakeKeyReportHistoryResponse>> getStakeReportHistory(
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(stakeKeyReportService.getStakeKeyReportHistory(
        userPrincipal.getUsername(), filterRequest, pagination.toPageable()));
  }

  @GetMapping(value = "/dashboard")
  @LogMessage
  @Operation(summary = "Get report history of stake key and pool id")
  public ResponseEntity<BaseFilterResponse<ReportHistoryResponse>> getReportHistory(
      HttpServletRequest request,
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "createdAt"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        reportHistoryService.getReportHistory(filterRequest, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/registrations")
  @LogMessage
  @Operation(summary = "Get stake registrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationFilterResponse>> getStakeKeyRegistrationsByReportId(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRegistrationsByReportId(reportId, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/de-registrations")
  @LogMessage
  @Operation(summary = "Get stake deregistrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationFilterResponse>> getStakeKeyDeregistrationsByReportId(
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDeRegistrationsByReportId(reportId,
            userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/delegations")
  @LogMessage
  @Operation(summary = "Get stake delegations by report id")
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getStakeKeyDelegationsByReportId(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDelegationsByReportId(reportId, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/rewards")
  @LogMessage
  @Operation(summary = "Get stake rewards by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getStakeKeyRewardsByReportId(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRewardsByReportId(reportId, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/withdrawals")
  @LogMessage
  @Operation(summary = "Get stake withdrawals by report id")
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getStakeKeyWithdrawalsByReportId(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeWithdrawalsByReportId(reportId, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/wallet-activity")
  @LogMessage
  @Operation(summary = "Get wallet activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivityByReportId(
      HttpServletRequest request,
      @PathVariable @Parameter(description = "The identifier of report") Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) @Valid Pagination pagination,
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        stakeKeyReportService.getWalletActivitiesByReportId(reportId, userPrincipal.getUsername(),
            pagination.toPageable()));
  }

  @GetMapping(value = "/report-limit")
  @LogMessage
  @Operation(summary = "Get report limit information")
  public ResponseEntity<ReportLimitResponse> getReportLimit(
      @AuthenticationPrincipal UserPrincipal userPrincipal) {
    return ResponseEntity.ok(reportHistoryService.getReportLimit(userPrincipal));
  }
}
