package org.cardanofoundation.explorer.api.model.response.pool.report;


import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PoolReportListResponse {
    private Long reportId;

    private String reportName;

    private Integer[] epochRanges;

    private Boolean isPoolSize;

    private Boolean isFreePaid;

    private Boolean eventRegistration;

    private Boolean eventDeregistration;

    private Boolean eventReward;

    private Boolean eventPoolUpdate;

    public static PoolReportListResponse toDomain(PoolReportHistory entity) {
        return PoolReportListResponse.builder()
                .reportId(entity.getId())
                .reportName(entity.getReportHistory().getReportName())
                .epochRanges(new Integer[]{entity.getBeginEpoch(), entity.getEndEpoch()})
                .isPoolSize(entity.getIsPoolSize())
                .isFreePaid(entity.getIsFeesPaid())
                .eventRegistration(entity.getEventRegistration())
                .eventDeregistration(entity.getEventDeregistration())
                .eventReward(entity.getEventReward())
                .eventPoolUpdate(entity.getEventPoolUpdate())
                .build();
    }
}
