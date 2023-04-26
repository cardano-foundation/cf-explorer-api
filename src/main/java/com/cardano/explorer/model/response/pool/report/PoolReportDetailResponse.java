package com.cardano.explorer.model.response.pool.report;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateProjection;
import lombok.*;

import java.math.BigDecimal;
import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PoolReportDetailResponse {

    private BaseFilterResponse<PoolRegistration> poolRegistrations;

    private BaseFilterResponse<PoolUpdate> poolUpdates;

    private BaseFilterResponse<RewardDistribution> rewardDistributions;

    private BaseFilterResponse<Deregistration> deregistrations;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class PoolRegistration {
        private String txnHash;

        private Date timestamp;

        private BigDecimal adaValueHold;

        private BigDecimal adaValueFees;

        private String owner;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class PoolUpdate {
        private String txnHash;

        private Date timestamp;

        private BigDecimal adaValueHold;

        private BigDecimal adaValueFees;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class RewardDistribution {
        private String epoch;

        private Date date;

        private BigDecimal operatorReward;

        private String rewardAccount;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class Deregistration {
        private String txnHash;

        private Date date;

        private BigDecimal adaValueHold;

        private BigDecimal adaValueFees;

        private String owner;
    }

}
