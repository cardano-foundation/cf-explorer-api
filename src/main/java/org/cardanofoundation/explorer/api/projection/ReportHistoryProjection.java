package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import java.time.LocalDateTime;

public interface ReportHistoryProjection {

  Long getStakeKeyReportId();

  Long getPoolReportId();

  String getReportName();

  LocalDateTime getCreatedAt();

  ReportStatus getStatus();

  ReportType getType();
}
