package org.cardanofoundation.explorer.api.model.response.stake.report;

import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

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
