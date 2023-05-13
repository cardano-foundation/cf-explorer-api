package org.cardanofoundation.explorer.api.model.request.pool.report;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
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

  public PoolReportHistory toEntity(ReportHistory reportHistory) {
    return PoolReportHistory.builder()
        .poolView(this.poolId)
        .isPoolSize(this.isPoolSize)
        .isFeesPaid(this.isFeesPaid)
        .beginEpoch(this.epochRanges[0])
        .endEpoch(this.epochRanges[1])
        .eventRegistration(this.isRegistration)
        .eventReward(this.isReward)
        .eventDeregistration(this.isDeregistration)
        .eventPoolUpdate(this.isPoolUpdate)
        .reportHistory(reportHistory)
        .build();
  }
}
