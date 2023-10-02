package org.cardanofoundation.explorer.api.service;


import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;

public interface HealthCheckService {

  SyncStatus getSyncStatus();
}
