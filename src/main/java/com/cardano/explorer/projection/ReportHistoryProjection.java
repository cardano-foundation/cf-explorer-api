package com.cardano.explorer.projection;

import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import java.time.LocalDateTime;

public interface ReportHistoryProjection {

  Long getStakeKeyReportId();

  Long getPoolReportId();

  String getReportName();

  LocalDateTime getCreatedAt();

  ReportStatus getStatus();

  ReportType getType();
}
