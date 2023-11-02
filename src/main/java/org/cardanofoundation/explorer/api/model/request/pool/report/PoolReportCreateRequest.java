package org.cardanofoundation.explorer.api.model.request.pool.report;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.ReportHistory;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PoolReportCreateRequest {

  private String reportName;

  @LengthValid(CommonConstant.POOL_VIEW_LENGTH)
  private String poolId;

  private Boolean isPoolSize;

  private Boolean isFeesPaid;

  private Boolean eventRegistration;

  private Boolean eventDeregistration;

  private Boolean eventReward;

  private Boolean eventPoolUpdate;

  private Integer[] epochRanges;

  public PoolReportHistory toEntity(ReportHistory reportHistory) {
    return PoolReportHistory.builder()
        .poolView(this.poolId)
        .isPoolSize(this.isPoolSize)
        .isFeesPaid(this.isFeesPaid)
        .beginEpoch(this.epochRanges[0])
        .endEpoch(this.epochRanges[1])
        .eventRegistration(this.eventRegistration)
        .eventReward(this.eventReward)
        .eventDeregistration(this.eventDeregistration)
        .eventPoolUpdate(this.eventPoolUpdate)
        .reportHistory(reportHistory)
        .build();
  }
}
