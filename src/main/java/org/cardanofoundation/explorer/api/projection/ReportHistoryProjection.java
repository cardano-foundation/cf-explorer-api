package org.cardanofoundation.explorer.api.projection;

import java.time.LocalDateTime;

import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

public interface ReportHistoryProjection {

  Long getStakeKeyReportId();

  Long getPoolReportId();

  String getReportName();

  LocalDateTime getCreatedAt();

  ReportStatus getStatus();

  ReportType getType();
}
