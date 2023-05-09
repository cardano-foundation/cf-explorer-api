package org.cardanofoundation.explorer.api.model.response.pool.report;


import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
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

    private Boolean isRegistration;

    private Boolean isDeregistration;

    private Boolean isReward;

    private Boolean isPoolUpdate;

    public static PoolReportListResponse toDomain(PoolReport entity) {
        return PoolReportListResponse.builder()
                .reportId(entity.getId())
                .reportName(entity.getReportName())
                .epochRanges(new Integer[]{entity.getBeginEpoch(), entity.getEndEpoch()})
                .isPoolSize(entity.getIsPoolSize())
                .isFreePaid(entity.getIsFeesPaid())
                .isRegistration(entity.getIsRegistration())
                .isDeregistration(entity.getIsDeregistration())
                .isReward(entity.getIsReward())
                .isPoolUpdate(entity.getIsPoolUpdate())
                .build();
    }
}
