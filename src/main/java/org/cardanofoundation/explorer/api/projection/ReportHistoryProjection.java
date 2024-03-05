package org.cardanofoundation.explorer.api.projection;

import java.time.LocalDateTime;

import org.cardanofoundation.explorer.common.entity.enumeration.ReportStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.ReportType;

public interface ReportHistoryProjection {

  Long getStakeKeyReportId();

  Long getPoolReportId();

  String getReportName();

  LocalDateTime getCreatedAt();

  ReportStatus getStatus();

  ReportType getType();
}
