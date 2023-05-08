package org.cardanofoundation.explorer.api.controller;

import jakarta.servlet.http.HttpServletRequest;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.request.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;


import lombok.RequiredArgsConstructor;

import org.springdoc.api.annotations.ParameterObject;

import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/staking-lifecycle/report")
@RequiredArgsConstructor
public class StakeKeyReportController {

  private final StakeKeyReportService stakeKeyReportService;

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
                                                                   @RequestParam(required = false) String fileExtension) {
    String username = request.getAttribute("username").toString();
    StakeKeyReportResponse response = stakeKeyReportService.exportStakeKeyReport(reportId, username,
                                                                                 fileExtension);
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
      @PathVariable String stakeKey,
      @ParameterObject Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey, username, pageable));
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
      @ParameterObject Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(stakeKeyReportService.getStakeKeyReportHistory(username, pageable));
  }

  @GetMapping(value = "/dashboard")
  @LogMessage
  @Operation(summary = "Get report history of stake key and pool id")
  public ResponseEntity<BaseFilterResponse<ReportHistoryResponse>> getReportHistory(
      HttpServletRequest request,
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "createdAt"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getReportHistory(filterRequest, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/registrations")
  @LogMessage
  @Operation(summary = "Get stake registrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyRegistrationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRegistrationsByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/de-registrations")
  @LogMessage
  @Operation(summary = "Get stake deregistrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyDeregistrationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDeRegistrationsByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/delegations")
  @LogMessage
  @Operation(summary = "Get stake delegations by report id")
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getStakeKeyDelegationsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDelegationsByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/rewards")
  @LogMessage
  @Operation(summary = "Get stake rewards by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getStakeKeyRewardsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRewardsByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/withdrawals")
  @LogMessage
  @Operation(summary = "Get stake withdrawals by report id")
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getStakeKeyWithdrawalsByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeWithdrawalsByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/wallet-activity")
  @LogMessage
  @Operation(summary = "Get wallet activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivityByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getWalletActivitiesByReportId(reportId, username, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/reward-activity")
  @LogMessage
  @Operation(summary = "Get reward activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardActivityResponse>> getRewardActivityByReportId(
      HttpServletRequest request,
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        stakeKeyReportService.getRewardActivitiesByReportId(reportId, username, pageable));
  }
}
