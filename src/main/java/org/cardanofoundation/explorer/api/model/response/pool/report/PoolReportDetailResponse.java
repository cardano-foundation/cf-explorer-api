package org.cardanofoundation.explorer.api.model.response.pool.report;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.util.report.ColumnFieldEnum;
import org.cardanofoundation.explorer.api.util.report.ColumnTitleEnum;
import org.cardanofoundation.explorer.api.util.report.ExportColumn;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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

    private BigDecimal size;

    public static EpochSize toDomain(PoolReportProjection projection) {
      return EpochSize.builder()
          .epoch(projection.getEpochNo().toString())
          .size(new BigDecimal(projection.getSize()))
          .build();
    }

    public static List<ExportColumn> designFile() {
      List<ExportColumn> epochSizeColumns = new ArrayList<>();
      epochSizeColumns.add(new ExportColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
      epochSizeColumns.add(new ExportColumn(ColumnFieldEnum.SIZE_COLUMN, ColumnTitleEnum.SIZE_TITLE));
      return epochSizeColumns;
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
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFee()));
      return result;
    }

    public static List<ExportColumn> designFile(boolean isFee) {
      List<ExportColumn> poolRegistrationsColumns = new ArrayList<>();
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        poolRegistrationsColumns.add(new ExportColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
            ColumnTitleEnum.ADA_VALUE_FEE_TITLE));
      }
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE));
      return poolRegistrationsColumns;
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

    public static List<ExportColumn> designFile(boolean isFee) {
      List<ExportColumn> poolUpdateColumns = new ArrayList<>();
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        poolUpdateColumns.add(new ExportColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
            ColumnTitleEnum.ADA_VALUE_FEE_TITLE));
      }
      return poolUpdateColumns;
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

    public static List<ExportColumn> designFile() {
      List<ExportColumn> rewardDistributionColumns = new ArrayList<>();
      rewardDistributionColumns.add(
          new ExportColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
      rewardDistributionColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.DATE_TITLE));
      rewardDistributionColumns.add(new ExportColumn(ColumnFieldEnum.OPERATOR_REWARD_COLUMN,
          ColumnTitleEnum.OPERATOR_REWARD_TITLE));
      rewardDistributionColumns.add(new ExportColumn(ColumnFieldEnum.REWARD_ACCOUNT_COLUMN,
          ColumnTitleEnum.REWARD_ACCOUNT_TITLE));
      return rewardDistributionColumns;
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
              .adaValueHold(new BigDecimal(response.getPoolHold()))
              .adaValueFee(new BigDecimal(response.getFee()))
              .owner(String.join("\n", response.getStakeKeys()))
              .build();
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFee()));
      return result;
    }

    public static List<ExportColumn> designFile(boolean isFee) {
      List<ExportColumn> deregistrationColumns = new ArrayList<>();
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.DATE_TITLE));
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        deregistrationColumns.add(new ExportColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
            ColumnTitleEnum.ADA_VALUE_FEE_TITLE));
      }
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE));
      return deregistrationColumns;
    }
  }

}
