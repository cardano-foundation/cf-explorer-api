package org.cardanofoundation.explorer.api.model.response.pool.report;


import java.sql.Timestamp;

import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;

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

    private Boolean isFeesPaid;

    private Boolean eventRegistration;

    private Boolean eventDeregistration;

    private Boolean eventReward;

    private Boolean eventPoolUpdate;

    private Timestamp createdAt;

    private ReportStatus status;

    public static PoolReportListResponse toDomain(PoolReportHistory entity) {
        return PoolReportListResponse.builder()
                .reportId(entity.getId())
                .reportName(entity.getReportHistory().getReportName())
                .epochRanges(new Integer[]{entity.getBeginEpoch(), entity.getEndEpoch()})
                .isPoolSize(entity.getIsPoolSize())
                .isFeesPaid(entity.getIsFeesPaid())
                .eventRegistration(entity.getEventRegistration())
                .eventDeregistration(entity.getEventDeregistration())
                .eventReward(entity.getEventReward())
                .eventPoolUpdate(entity.getEventPoolUpdate())
                .createdAt(entity.getReportHistory().getCreatedAt())
                .status(entity.getReportHistory().getStatus())
                .build();
    }
}
