package org.cardanofoundation.explorer.api.model.request.pool.report;

import org.cardanofoundation.explorer.api.common.enumeration.PoolReportEvent;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import java.util.Arrays;
import java.util.stream.Collectors;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PoolReportCreateRequest {

  private String reportName;

  private String poolId;

  private Boolean isPoolSize;

  private Boolean isFeesPaid;

  private Integer[] epochRanges;

  private String[] event;

  public PoolReport toEntity(ReportHistory reportHistory, String username) {
    return PoolReport.builder()
        .poolView(this.poolId)
        .isPoolSize(this.isPoolSize)
        .isFeesPaid(this.isFeesPaid)
        .reportName(this.reportName)
        .beginEpoch(this.epochRanges[0])
        .endEpoch(this.epochRanges[1])
        .event(String.join(",", event))
        .reportHistory(reportHistory)
        .username(username)
        .build();
  }
}
