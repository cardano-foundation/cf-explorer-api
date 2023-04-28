package com.cardano.explorer.model.response.pool.report;


import com.sotatek.cardano.common.entity.PoolReport;
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

    private String event;

    public static PoolReportListResponse toDomain(PoolReport entity) {
        return PoolReportListResponse.builder()
                .reportId(entity.getId())
                .reportName(entity.getReportName())
                .epochRanges(new Integer[]{entity.getBeginEpoch(), entity.getEndEpoch()})
                .isPoolSize(entity.getIsPoolSize())
                .isFreePaid(entity.getIsFeesPaid())
                .event(entity.getEvent())
                .build();
    }
}
