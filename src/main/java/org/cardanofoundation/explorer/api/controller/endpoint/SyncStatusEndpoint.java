package org.cardanofoundation.explorer.api.controller.endpoint;

import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;
import org.cardanofoundation.explorer.api.service.HealthCheckService;

import org.springframework.boot.actuate.endpoint.annotation.Endpoint;
import org.springframework.boot.actuate.endpoint.annotation.ReadOperation;
import org.springframework.context.annotation.Bean;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;

import lombok.RequiredArgsConstructor;

@Endpoint(id = "sync-status")
@RequiredArgsConstructor
@Component
public class SyncStatusEndpoint {

  private final HealthCheckService healthCheckService;

  @ReadOperation
  @Bean
  public ResponseEntity<SyncStatus> checkSyncStatus() {
    var syncStatus = healthCheckService.getSyncStatus();
    if (Boolean.FALSE.equals(syncStatus.getIsSyncing())) {
      return ResponseEntity.internalServerError()
          .body(syncStatus);
    }

    return ResponseEntity.ok().body(syncStatus);
  }
}
