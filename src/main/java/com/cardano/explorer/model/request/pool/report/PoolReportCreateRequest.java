package com.cardano.explorer.model.request.pool.report;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import com.sotatek.cardano.common.entity.PoolReport;
import com.sotatek.cardano.common.entity.ReportHistory;
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

  private PoolReportEvent[] event;

  public PoolReport toEntity(ReportHistory reportHistory) {
    return PoolReport.builder()
        .poolView(this.poolId)
        .isPoolSize(this.isPoolSize)
        .isFeesPaid(this.isFeesPaid)
        .reportName(this.reportName)
        .beginEpoch(this.epochRanges[0])
        .endEpoch(this.epochRanges[1])
        .event(Arrays.stream(this.event).map(PoolReportEvent::getValue)
            .collect(Collectors.joining(",")))
        .reportHistory(reportHistory)
        .build();
  }
}
