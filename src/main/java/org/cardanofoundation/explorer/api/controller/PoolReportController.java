package org.cardanofoundation.explorer.api.controller;

import jakarta.servlet.http.HttpServletRequest;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.common.validate.pagination.Pagination;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;

import lombok.RequiredArgsConstructor;

import org.springdoc.core.annotations.ParameterObject;

import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("api/v1/pool-report")
@RequiredArgsConstructor
public class PoolReportController {

  private final PoolReportService poolReportService;

  @PostMapping("create")
  public ResponseEntity<Boolean> createPoolReport(@RequestBody PoolReportCreateRequest body,
                                                  HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.create(body, username));
  }

  @GetMapping("list")
  public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0, sort = {"id"},
          direction = Sort.Direction.DESC) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.list(pagination.toPageable(), username));
  }

  @GetMapping("detail/{reportId}/epoch-size")
  public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchEpochSize(reportId, pagination.toPageable(), username));
  }

  @GetMapping("detail/{reportId}/export")
  public ResponseEntity<Resource> export(@PathVariable Long reportId,
                                         @RequestParam(required = false) ExportType exportType,
                                         HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    PoolReportExportResponse response = poolReportService.export(reportId, exportType, username);
    return ResponseEntity.ok()
        .contentLength(response.getByteArrayInputStream().available())
        .header(HttpHeaders.CONTENT_DISPOSITION,
                "attachment; filename=\"" + response.getFileName() + "\"")
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .body(new InputStreamResource(response.getByteArrayInputStream()));
  }

  @GetMapping(value = "detail/{reportId}/pool-registration")
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> detailPoolRegistration(
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchPoolRegistration(reportId, pagination.toPageable(), username));
  }

  @GetMapping(value = "detail/{reportId}/pool-update")
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> detailPoolUpdate(
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchPoolUpdate(reportId, pagination.toPageable(), username));
  }

  @GetMapping(value = "detail/{reportId}/rewards-distribution")
  public ResponseEntity<BaseFilterResponse<RewardResponse>> detailRewardsDistribution(
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(
        poolReportService.fetchRewardsDistribution(reportId, pagination.toPageable(), username));
  }

  @GetMapping(value = "detail/{reportId}/deregistration")
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> detailDeregistration(
      @PathVariable Long reportId,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchDeregistraion(reportId, pagination.toPageable(), username));
  }

  @GetMapping("detail/{reportId}")
  public ResponseEntity<PoolReportHistory> detailPoolReport(@PathVariable Long reportId,
                                                     HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.detail(reportId, username));
  }

}
