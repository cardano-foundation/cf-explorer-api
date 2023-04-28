package com.cardano.explorer.controller;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.cardano.explorer.service.PoolReportService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Compose Pool Report file for each poolId, List all report for each Wallet User
 *
 * @author  an.nguyen4
 * @version 0.0.1
 * @since   26/04/2023
 */

@RestController
@RequestMapping("api/v1/pool-report")
@RequiredArgsConstructor
public class PoolReportController {

    private final PoolReportService poolReportService;

    @PostMapping("create")
    public ResponseEntity<Boolean> createPoolReport(@RequestBody PoolReportCreateRequest poolReportCreateRequest) {
        return ResponseEntity.ok(poolReportService.create(poolReportCreateRequest));
    }

    @GetMapping("list")
    public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(@ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
        return ResponseEntity.ok(poolReportService.list(pageable));
    }

    @GetMapping("detail/{reportId}")
    public ResponseEntity<PoolReportDetailResponse> detailPoolReport(@PathVariable String reportId, @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
        return ResponseEntity.ok(poolReportService.detail(reportId, pageable));
    }

    @GetMapping("detail/{reportId}/epoch-size")
    public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(@PathVariable String reportId, @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
        return ResponseEntity.ok(poolReportService.detailEpochSize(reportId, pageable));
    }
}
