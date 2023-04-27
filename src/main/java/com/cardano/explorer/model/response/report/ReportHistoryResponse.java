package com.cardano.explorer.model.response.report;

import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class ReportHistoryResponse {
  private Long id;
  private LocalDateTime createdAt;
  private String reportName;
  private ReportStatus status;
  private ReportType type;
}
