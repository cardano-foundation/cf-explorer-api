package org.cardanofoundation.explorer.api.model.response.pool.report;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.util.DataUtil;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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

    public static EpochSize toDomain(PoolReportProjection projection) {
      return EpochSize.builder()
          .epoch(projection.getEpochNo().toString())
          .fee(projection.getFee())
          .size(new BigDecimal(projection.getSize()))
          .build();
    }

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

    public static PoolRegistration toDomain(TabularRegisResponse response) {
      PoolRegistration result = PoolRegistration.builder()
          .txHash(response.getTxHash())
          .time(response.getTime())
          .adaValueHold(new BigDecimal(response.getDeposit()))
          .adaValueFee(new BigDecimal(response.getFee()))
          .owner(String.join("\n", response.getStakeKeys()))
          .build();
      result.setAdaValue(new BigDecimal(response.getTotalFee()));
      return result;
    }
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

    public static PoolUpdate toDomain(PoolUpdateDetailResponse response) {
      PoolUpdate result = PoolUpdate.builder()
          .txHash(response.getTxHash())
          .time(response.getTime())
          .adaValueHold(new BigDecimal(response.getPledge()))
          .adaValueFee(new BigDecimal(response.getFee()))
          .build();
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFee()));
      return result;
    }

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

    public static RewardDistribution toDomain(RewardResponse response) {
      return RewardDistribution.builder()
          .epoch(response.getEpochNo().toString())
          .time(response.getTime())
          .operatorReward(new BigDecimal(response.getAmount()))
          .rewardAccount(response.getRewardAccount())
          .build();
    }

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

    public static Deregistration toDomain(DeRegistrationResponse response) {
      Deregistration result = Deregistration.builder()
          .txHash(response.getTxHash())
          .time(response.getTime())
          .adaValueHold(DataUtil.isNullOrEmpty(response.getPoolHold()) ? null : new BigDecimal(
              response.getPoolHold()))
          .adaValueFee(new BigDecimal(response.getFee()))
          .owner(String.join("\n", response.getStakeKeys()))
          .build();

      result.setAdaValue(new BigDecimal(response.getTotalFee()));
      return result;
    }

  }

}
