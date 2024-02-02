package org.cardanofoundation.explorer.api.model.response.stake.report;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

@Getter
@Setter
@Builder
public class ReportHistoryResponse {
  private Long stakeKeyReportId;
  private Long poolReportId;
  private LocalDateTime createdAt;
  private String reportName;
  private ReportStatus status;
  private ReportType type;
}
