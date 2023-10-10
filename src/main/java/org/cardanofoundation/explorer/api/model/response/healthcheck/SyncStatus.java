package org.cardanofoundation.explorer.api.model.response.healthcheck;

import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonInclude;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class SyncStatus {
  Boolean isSyncing;
  String message;
  LocalDateTime latestBlockInsertTime;
}
