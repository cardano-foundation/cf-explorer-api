package org.cardanofoundation.explorer.api.model.response.pool.report;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PoolReportDetailResponse {

  private BaseFilterResponse<EpochSize> epochSizes;

  private BaseFilterResponse<TabularRegisResponse> poolRegistrations;

  private BaseFilterResponse<PoolUpdateDetailResponse> poolUpdates;

  private BaseFilterResponse<RewardResponse> rewardDistributions;

  private BaseFilterResponse<DeRegistrationResponse> deregistrations;

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @Builder
  public static class EpochSize {

    private String epoch;

    private BigInteger fee;

    private BigDecimal size;

    public static EpochSize toDomain(PoolHistoryKoiosProjection projection) {
      return EpochSize.builder()
          .epoch(projection.getEpochNo().toString())
          .size(new BigDecimal(projection.getActiveStake()))
          .fee(projection.getPoolFees())
          .build();
    }
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @Builder
  public static class PoolRegistration {

    private String txHash;

    private Date time;

    private BigDecimal adaValueHold;

    private BigDecimal adaValueFee;

    private BigDecimal adaValue;

    private String owner;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @Builder
  public static class PoolUpdate {

    private String txHash;

    private Date time;

    private BigDecimal adaValueHold;

    private BigDecimal adaValueFee;

    private BigDecimal adaValue;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @Builder
  public static class RewardDistribution {

    private String epoch;

    private Date time;

    private BigDecimal operatorReward;

    private String rewardAccount;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @Builder
  public static class Deregistration {

    private String txHash;

    private Date time;

    private BigDecimal adaValueHold;

    private BigDecimal adaValueFee;

    private BigDecimal adaValue;

    private String owner;
  }
}
