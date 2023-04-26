package com.cardano.explorer.model.request.pool.report;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PoolReportCreateRequest {
    private String reportName;

    private String poolId;

    private Integer[] epochRanges;

    private PoolReportEvent[] event;
}
