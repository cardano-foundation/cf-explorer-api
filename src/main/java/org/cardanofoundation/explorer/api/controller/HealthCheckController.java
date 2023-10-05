package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;
import org.cardanofoundation.explorer.api.service.HealthCheckService;

@RestController
@RequestMapping("/api/v1/health-check")
@RequiredArgsConstructor
@Tag(name = "healthcheck", description = "The Sync-HeathCheck APIs")
public class HealthCheckController {

  private final HealthCheckService healthCheckService;

  @GetMapping("/sync-status")
  @LogMessage
  @Operation(summary = "check sync status", tags = {"healthcheck"})
  public ResponseEntity<SyncStatus> checkSyncStatus() {
    var syncStatus = healthCheckService.getSyncStatus();
    if (Boolean.FALSE.equals(syncStatus.getIsSyncing())) {
      return ResponseEntity.internalServerError()
          .body(syncStatus);
    }

    return ResponseEntity.ok().body(syncStatus);
  }

}
