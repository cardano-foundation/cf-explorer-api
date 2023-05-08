package org.cardanofoundation.explorer.api.controller;

import com.google.gson.Gson;
import jakarta.servlet.http.HttpServletRequest;
import org.cardanofoundation.explorer.api.config.LogMessage;
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
import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
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
  public ResponseEntity<Boolean> createPoolReport(@RequestBody  String body, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.create(new Gson().fromJson(body, PoolReportCreateRequest.class), username));
  }

  @GetMapping("list")
  public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(
      @ParameterObject @PageableDefault(size = 10, page = 0, sort = {"id"},
          direction = Sort.Direction.DESC) Pageable pageable,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.list(pageable, username));
  }

  @GetMapping("detail/{reportId}/full")
  public ResponseEntity<PoolReportDetailResponse> detailFullPoolReport(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.detailFull(reportId, pageable, username));
  }

  @GetMapping("detail/{reportId}/epoch-size")
  public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(
      @PathVariable String reportId,
      @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable,
      HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.detailEpochSize(reportId, pageable, username));
  }

  @GetMapping("detail/{reportId}/export")
  public ResponseEntity<Resource> export(@PathVariable Long reportId,
      @RequestParam(required = false) String fileExtension, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    PoolReportExportResponse response = poolReportService.export(reportId, fileExtension, username);
    return ResponseEntity.ok()
        .contentLength(response.getByteArrayInputStream().available())
        .header(HttpHeaders.CONTENT_DISPOSITION,
            "attachment; filename=\"" + response.getFileName() + "\"")
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .body(new InputStreamResource(response.getByteArrayInputStream()));
  }

  @GetMapping(value = "detail/{reportId}/pool-registration")
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> detailPoolRegistration(
          @PathVariable String reportId,
          @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchPoolRegistration(reportId, pageable, username));
  }

  @GetMapping(value = "detail/{reportId}/pool-update")
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> detailPoolUpdate(
          @PathVariable String reportId,
          @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchPoolUpdate(reportId, pageable, username));
  }

  @GetMapping(value = "detail/{reportId}/rewards-distribution")
  public ResponseEntity<BaseFilterResponse<RewardResponse>> detailRewardsDistribution(
          @PathVariable String reportId,
          @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchRewardsDistribution(reportId, pageable, username));
  }

  @GetMapping(value = "detail/{reportId}/deregistration")
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> detailDeregistration(
          @PathVariable String reportId,
          @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable, HttpServletRequest request) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.fetchDeregistraion(reportId, pageable, username));
  }

  @GetMapping("detail/{reportId}")
  public ResponseEntity<PoolReport> detailPoolReport(@PathVariable String reportId, HttpServletRequest request
  ) {
    String username = request.getAttribute("username").toString();
    return ResponseEntity.ok(poolReportService.detail(reportId, username));
  }

}
