package org.cardanofoundation.explorer.api.model.request.pool.report;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.PoolReportEvent;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import java.util.Arrays;
import java.util.stream.Collectors;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PoolReportCreateRequest {

  private String reportName;

  private String poolId;

  private Boolean isPoolSize;

  private Boolean isFeesPaid;

  private Boolean isRegistration;

  private Boolean isDeregistration;

  private Boolean isReward;

  private Boolean isPoolUpdate;

  private Integer[] epochRanges;

  public PoolReport toEntity(ReportHistory reportHistory, String username) {
    return PoolReport.builder()
        .poolView(this.poolId)
        .isPoolSize(this.isPoolSize)
        .isFeesPaid(this.isFeesPaid)
        .reportName(this.reportName)
        .beginEpoch(this.epochRanges[0])
        .endEpoch(this.epochRanges[1])
        .isRegistration(this.isRegistration)
        .isReward(this.isReward)
        .isDeregistration(this.isDeregistration)
        .isPoolUpdate(this.isPoolUpdate)
        .reportHistory(reportHistory)
        .username(username)
        .build();
  }
}
