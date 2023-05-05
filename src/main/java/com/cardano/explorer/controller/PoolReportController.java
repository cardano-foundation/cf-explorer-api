package com.cardano.explorer.controller;

import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.lifecycle.TabularRegisResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportExportResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.cardano.explorer.service.PoolReportService;
import com.sotatek.cardano.common.entity.PoolReport;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Compose Pool Report file for each poolId, List all report for each Wallet User
 *
 * @author an.nguyen4
 * @version 0.0.1
 * @since 26/04/2023
 */

@RestController
@RequestMapping("api/v1/pool-report")
@RequiredArgsConstructor
public class PoolReportController {

  private final PoolReportService poolReportService;

  @PostMapping("create")
  public ResponseEntity<Boolean> createPoolReport(
      @RequestBody PoolReportCreateRequest poolReportCreateRequest) {
    return ResponseEntity.ok(poolReportService.create(poolReportCreateRequest));
  }

  @GetMapping("list")
  public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.list(pageable));
  }

  @GetMapping("detail/{reportId}/full")
  public ResponseEntity<PoolReportDetailResponse> detailFullPoolReport(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailFull(reportId, pageable));
  }

  @GetMapping("detail/{reportId}/epoch-size")
  public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailEpochSize(reportId, pageable));
  }

  @GetMapping("detail/{reportId}/export")
  public ResponseEntity<Resource> export(@PathVariable Long reportId) {
    //TODO add processing
    PoolReportExportResponse response = poolReportService.export(reportId);
    return ResponseEntity.ok()
        .contentLength(response.getByteArrayInputStream().available())
        .header(HttpHeaders.CONTENT_DISPOSITION,
            "attachment; filename=\"" + response.getFileName() + ".csv\"")
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .body(new InputStreamResource(response.getByteArrayInputStream()));
  }

  @GetMapping(value = "detail/{reportId}/pool-registration")
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> detailPoolRegistration(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailPoolRegistration(reportId, pageable));
  }

  @GetMapping(value = "detail/{reportId}/pool-update")
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> detailPoolUpdate(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailPoolUpdate(reportId, pageable));
  }

  @GetMapping(value = "detail/{reportId}/rewards-distribution")
  public ResponseEntity<BaseFilterResponse<RewardResponse>> detailRewardsDistribution(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailRewardsDistribution(reportId, pageable));
  }

  @GetMapping(value = "detail/{reportId}/deregistration")
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> detailDeregistration(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
    return ResponseEntity.ok(poolReportService.detailDeregistraion(reportId, pageable));
  }

  @GetMapping("detail/{reportId}")
  public ResponseEntity<PoolReport> detailPoolReport(@PathVariable String reportId
  ) {
    return ResponseEntity.ok(poolReportService.detail(reportId));
  }

}
