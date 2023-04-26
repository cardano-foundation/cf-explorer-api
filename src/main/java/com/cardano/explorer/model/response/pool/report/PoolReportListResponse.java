package com.cardano.explorer.model.response.pool.report;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PoolReportListResponse {
    private String reportName;

    private Integer[] epochRanges;

    private String event;
}
