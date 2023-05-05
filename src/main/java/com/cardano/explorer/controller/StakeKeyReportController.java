package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.report.ReportHistoryFilterRequest;
import com.cardano.explorer.model.request.report.StakeKeyReportRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.report.ReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWalletActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.service.StakeKeyReportService;
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
      @RequestBody StakeKeyReportRequest stakeKeyReportRequest) {
    return ResponseEntity.ok().body(stakeKeyReportService.generateStakeKeyReport(
        stakeKeyReportRequest));
  }

  @GetMapping(value = "/stake-key/{reportId}/export")
  @LogMessage
  @Operation(summary = "Export stake key report by id")
  public ResponseEntity<Resource> exportStakeKeyReportByStorageKey(@PathVariable Long reportId,
      @RequestParam(required = false) String fileExtension) {
    StakeKeyReportResponse response = stakeKeyReportService.exportStakeKeyReport(reportId,
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
      @PathVariable String stakeKey,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/detail")
  @LogMessage
  @Operation(summary = "Get stake key report detail by report id")
  public ResponseEntity<StakeKeyReportHistoryResponse> getStakeReportDetail(
      @PathVariable Long reportId) {
    return ResponseEntity.ok(stakeKeyReportService.getStakeKeyReportHistoryByReportId(reportId));
  }

  @GetMapping(value = "/stake-key/history")
  @LogMessage
  @Operation(summary = "Get all stake key report history")
  public ResponseEntity<BaseFilterResponse<StakeKeyReportHistoryResponse>> getStakeReportHistory(
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(stakeKeyReportService.getStakeKeyReportHistory(pageable));
  }

  @GetMapping(value = "/dashboard")
  @LogMessage
  @Operation(summary = "Get report history of stake key and pool id")
  public ResponseEntity<BaseFilterResponse<ReportHistoryResponse>> getReportHistory(
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "createdAt"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyReportService.getReportHistory(filterRequest, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/registrations")
  @LogMessage
  @Operation(summary = "Get stake registrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyRegistrationsByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeRegistrationsByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/de-registrations")
  @LogMessage
  @Operation(summary = "Get stake deregistrations by report id")
  public ResponseEntity<BaseFilterResponse<StakeRegistrationLifeCycle>> getStakeKeyDeregistrationsByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDeRegistrationsByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/delegations")
  @LogMessage
  @Operation(summary = "Get stake delegations by report id")
  public ResponseEntity<BaseFilterResponse<StakeDelegationFilterResponse>> getStakeKeyDelegationsByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeDelegationsByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/rewards")
  @LogMessage
  @Operation(summary = "Get stake rewards by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardResponse>> getStakeKeyRewardsByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(stakeKeyReportService.getStakeRewardsByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/withdrawals")
  @LogMessage
  @Operation(summary = "Get stake withdrawals by report id")
  public ResponseEntity<BaseFilterResponse<StakeWithdrawalFilterResponse>> getStakeKeyWithdrawalsByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getStakeWithdrawalsByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/wallet-activity")
  @LogMessage
  @Operation(summary = "Get wallet activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeWalletActivityResponse>> getWalletActivityByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getWalletActivitiesByReportId(reportId, pageable));
  }

  @GetMapping(value = "/stake-key/{reportId}/reward-activity")
  @LogMessage
  @Operation(summary = "Get reward activity by report id")
  public ResponseEntity<BaseFilterResponse<StakeRewardActivityResponse>> getRewardActivityByReportId(
      @PathVariable Long reportId,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "time"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(
        stakeKeyReportService.getRewardActivitiesByReportId(reportId, pageable));
  }
}
