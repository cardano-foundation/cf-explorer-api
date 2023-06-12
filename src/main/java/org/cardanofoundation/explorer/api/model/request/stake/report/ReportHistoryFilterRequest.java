package org.cardanofoundation.explorer.api.model.request.stake.report;

import java.util.Date;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;

@Getter
@Setter
@Builder
public class ReportHistoryFilterRequest {
  @DateValid
  private Date fromDate;
  @DateValid
  private Date toDate;
  private String reportName;
}
