package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;

import io.swagger.v3.oas.annotations.Parameter;
import jakarta.validation.Valid;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.interceptor.auth.UserPrincipal;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;

import lombok.RequiredArgsConstructor;

import org.springdoc.core.annotations.ParameterObject;

import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("api/v1/pool-report")
@RequiredArgsConstructor
@Validated
@Tag(name = "pool-report", description = "The pool lifecycle report APIs")
public class PoolReportController {

  private final PoolReportService poolReportService;

  @PostMapping("create")
  @LogMessage
  @Operation(summary = "Create report for pool lifecycle", tags = {"pool-report"})
  public ResponseEntity<Boolean> createPoolReport(@RequestBody PoolReportCreateRequest body,
                                                  @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.create(body, userPrincipal));
  }

  @GetMapping("list")
  @LogMessage
  @Operation(summary = "Get list pool report by user", tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(
      @RequestAttribute("user") UserPrincipal userPrincipal,
      @ParameterObject @Parameter(description = "filter condition") ReportHistoryFilterRequest filterRequest,
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(poolReportService.list(pagination.toPageable(),
        userPrincipal.getUsername(), filterRequest));
  }

  @GetMapping("detail/{reportId}/epoch-size")
  @LogMessage
  @Operation(
      summary = "Get epoch size of a pool report",
      tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.fetchEpochSize(reportId, pagination.toPageable(),
        userPrincipal.getUsername()));
  }

  @GetMapping("detail/{reportId}/export")
  @LogMessage
  @Operation(summary = "Export pool report", tags = {"pool-report"})
  public ResponseEntity<Resource> export(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @RequestParam(required = false) @Parameter(description = "Type for export") ExportType exportType,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    PoolReportExportResponse response = poolReportService.export(reportId, exportType,
        userPrincipal.getUsername());
    return ResponseEntity.ok()
        .contentLength(response.getByteArrayInputStream().available())
        .header(HttpHeaders.CONTENT_DISPOSITION,
                "attachment; filename=\"" + response.getFileName() + "\"")
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .body(new InputStreamResource(response.getByteArrayInputStream()));
  }

  @GetMapping(value = "detail/{reportId}/pool-registration")
  @LogMessage
  @Operation(summary = "Get pool registration of a pool report", tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> detailPoolRegistration(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.fetchPoolRegistration(reportId, pagination.toPageable(), userPrincipal.getUsername()));
  }

  @GetMapping(value = "detail/{reportId}/pool-update")
  @LogMessage
  @Operation(summary = "Get pool update of a pool report", tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> detailPoolUpdate(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.fetchPoolUpdate(reportId, pagination.toPageable(),
        userPrincipal.getUsername()));
  }

  @GetMapping(value = "detail/{reportId}/rewards-distribution")
  @LogMessage
  @Operation(summary = "Get rewards distribution of a pool report", tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<RewardResponse>> detailRewardsDistribution(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(
        poolReportService.fetchRewardsDistribution(reportId, pagination.toPageable(),
            userPrincipal.getUsername()));
  }

  @GetMapping(value = "detail/{reportId}/deregistration")
  @LogMessage
  @Operation(summary = "Get deregistration of a pool report", tags = {"pool-report"})
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> detailDeregistration(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.fetchDeregistraion(reportId, pagination.toPageable(),
        userPrincipal.getUsername()));
  }

  @GetMapping("detail/{reportId}")
  @LogMessage
  @Operation(summary = "Get detail information of a pool report", tags = {"pool-report"})
  public ResponseEntity<PoolReportHistory> detailPoolReport(
      @PathVariable @Parameter(description = "The identifier of the report") Long reportId,
      @RequestAttribute("user") UserPrincipal userPrincipal) {
    return ResponseEntity.ok(poolReportService.detail(reportId, userPrincipal.getUsername()));
  }

}
