package org.cardanofoundation.explorer.api.model.request.stake.report;

import java.util.Date;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class ReportHistoryFilterRequest {
  private Date fromDate;
  private Date toDate;
  private String reportName;
}
