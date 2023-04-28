package com.cardano.explorer.model.response.pool.report;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.PoolReportProjection;
import com.cardano.explorer.model.response.pool.projection.PoolUpdateProjection;
import com.fasterxml.jackson.databind.ser.Serializers;
import lombok.*;

import java.math.BigDecimal;
import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PoolReportDetailResponse {

    private BaseFilterResponse<EpochSize>  epochSizes;

    private BaseFilterResponse<PoolRegistration> poolRegistrations;

    private BaseFilterResponse<PoolUpdate> poolUpdates;

    private BaseFilterResponse<RewardDistribution> rewardDistributions;

    private BaseFilterResponse<Deregistration> deregistrations;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class EpochSize {
        private String epoch;

        private BigDecimal size;

        public static EpochSize toDomain(PoolReportProjection projection) {
            return EpochSize.builder()
                    .epoch(projection.getEpochNo().toString())
                    .size(new BigDecimal(projection.getSize()))
                    .build();
        }
    }

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

        public static PoolRegistration toDomain(PoolReportProjection projection) {
            return PoolRegistration.builder()
                    .txnHash(projection.getTxnHash())
                    .timestamp(projection.getTimestamp())
                    .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
                    .adaValueFees(new BigDecimal(projection.getAdaValueFees()))
                    .owner(projection.getOwner())
                    .build();
        }
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

        public static PoolUpdate toDomain(PoolReportProjection projection) {
            return PoolUpdate.builder()
                    .txnHash(projection.getTxnHash())
                    .timestamp(projection.getTimestamp())
                    .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
                    .adaValueFees(new BigDecimal(projection.getAdaValueFees()))
                    .build();
        }
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

        public static RewardDistribution toDomain(PoolReportProjection projection) {
            return RewardDistribution.builder()
                    .epoch(projection.getEpochNo().toString())
                    .date(projection.getTimestamp())
                    .operatorReward(new BigDecimal(projection.getOperatorReward()))
                    .rewardAccount(projection.getRewardAccount())
                    .build();
        }
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

        public static Deregistration toDomain(PoolReportProjection projection) {
            return Deregistration.builder()
                    .txnHash(projection.getTxnHash())
                    .date(projection.getTimestamp())
                    .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
                    .adaValueFees(new BigDecimal(projection.getAdaValueFees()))
                    .owner(projection.getOwner())
                    .build();
        }
    }

}
