package org.cardanofoundation.explorer.api.controller;

import jakarta.servlet.http.HttpServletRequest;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.StakeKeyLengthValid;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;


import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.common.validation.date.param.DateValid;
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
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/staking-lifecycle/report")
@RequiredArgsConstructor
@Validated
public class StakeKeyReportController {

  private final StakeKeyReportService stakeKeyReportService;
  private final ReportHistoryService reportHistoryService;

  @PostMapping(value = "/stake-key")
  @LogMessage
  @Operation(summary = "Generate stake key report")
  public ResponseEntity<StakeKeyReportHistoryResponse> generateStakeKeyReport(
      HttpServletRequest request,
      @RequestBody StakeKeyReportRequest stakeKeyReportRequest) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok()
        .body(stakeKeyReportService.generateStakeKeyReport(stakeKeyReportRequest, username));
  }

  @GetMapping(value = "/stake-key/{reportId}/export")
  @LogMessage
  @Operation(summary = "Export stake key report by id")
  public ResponseEntity<Resource> exportStakeKeyReportByStorageKey(HttpServletRequest request,
                                                                   @PathVariable Long reportId,
                                                                   @RequestParam(required = false) ExportType exportType) {
    String username = request.getAttribute("username").toString();
    StakeKeyReportResponse response = stakeKeyReportService.exportStakeKeyReport(reportId, username,
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
      HttpServletRequest request,
      @PathVariable @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY) @StakeKeyLengthValid String stakeKey,
      @ParameterObject @PaginationValid Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/detail")
  @LogMessage
  @Operation(summary = "Get stake key report detail by report id")
  public ResponseEntity<StakeKeyReportHistoryResponse> getStakeReportDetail(
      HttpServletRequest request,
      @PathVariable Long reportId) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByReportId(reportId, username));
  }

  @GetMapping(value = "/stake-key/history")
  @LogMessage
  @Operation(summary = "Get all stake key report history")
  public ResponseEntity<BaseFilterResponse<StakeKeyReportHistoryResponse>> getStakeReportHistory(
      HttpServletRequest request,
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(stakeKeyReportService.getStakeKeyReportHistory(username, filterRequest, pagination.toPageable()));
  }

  @GetMapping(value = "/dashboard")
  @LogMessage
  @Operation(summary = "Get report history of stake key and pool id")
  public ResponseEntity<BaseFilterResponse<ReportHistoryResponse>> getReportHistory(
      HttpServletRequest request,
      @ParameterObject @Parameter(description = "filter condition") @DateValid ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "createdAt"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        reportHistoryService.getReportHistory(filterRequest, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/registrations")
  @LogMessage
  @Operation(summary = "Get stake registrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyRegistrationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRegistrationsByReportId(reportId, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/de-registrations")
  @LogMessage
  @Operation(summary = "Get stake deregistrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyDeregistrationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDeRegistrationsByReportId(reportId, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/delegations")
  @LogMessage
  @Operation(summary = "Get stake delegations by report id")
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getStakeKeyDelegationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDelegationsByReportId(reportId, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/rewards")
  @LogMessage
  @Operation(summary = "Get stake rewards by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getStakeKeyRewardsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRewardsByReportId(reportId, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/withdrawals")
  @LogMessage
  @Operation(summary = "Get stake withdrawals by report id")
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getStakeKeyWithdrawalsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeWithdrawalsByReportId(reportId, username, pagination.toPageable()));
  }

  @GetMapping(value = "/stake-key/{reportId}/wallet-activity")
  @LogMessage
  @Operation(summary = "Get wallet activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivityByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pagination pagination) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getWalletActivitiesByReportId(reportId, username, pagination.toPageable()));
  }
}
