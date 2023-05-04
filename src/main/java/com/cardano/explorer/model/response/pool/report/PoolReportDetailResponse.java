package com.cardano.explorer.model.response.pool.report;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.projection.PoolReportProjection;
import com.cardano.explorer.util.csv.CSVColumn;
import com.cardano.explorer.util.csv.ColumnFieldEnum;
import com.cardano.explorer.util.csv.ColumnTitleEnum;
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

    public static List<CSVColumn> designFile() {
      List<CSVColumn> epochSizeColumns = new ArrayList<>();
      epochSizeColumns.add(new CSVColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
      epochSizeColumns.add(new CSVColumn(ColumnFieldEnum.SIZE_COLUMN, ColumnTitleEnum.SIZE_TITLE));
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

    public static PoolRegistration toDomain(PoolReportProjection projection) {
      PoolRegistration result = PoolRegistration.builder()
          .txHash(projection.getTxnHash())
          .time(projection.getTimestamp())
          .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
          .adaValueFee(new BigDecimal(projection.getAdaValueFees()))
          .owner(projection.getOwner())
          .build();
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFee()));
      return result;
    }

    public static List<CSVColumn> designFile(boolean isFee) {
      List<CSVColumn> poolRegistrationsColumns = new ArrayList<>();
      poolRegistrationsColumns.add(
          new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      poolRegistrationsColumns.add(
          new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
      poolRegistrationsColumns.add(
          new CSVColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        poolRegistrationsColumns.add(new CSVColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
            ColumnTitleEnum.ADA_VALUE_FEE_TITLE));
      }
      poolRegistrationsColumns.add(
          new CSVColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE));
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

    private BigDecimal adaValueFees;

    private BigDecimal adaValue;

    public static PoolUpdate toDomain(PoolReportProjection projection) {
      PoolUpdate result = PoolUpdate.builder()
          .txHash(projection.getTxnHash())
          .time(projection.getTimestamp())
          .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
          .adaValueFees(new BigDecimal(projection.getAdaValueFees()))
          .build();
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFees()));
      return result;
    }

    public static List<CSVColumn> designFile(boolean isFee) {
      List<CSVColumn> poolUpdateColumns = new ArrayList<>();
      poolUpdateColumns.add(
          new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      poolUpdateColumns.add(
          new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
      poolUpdateColumns.add(
          new CSVColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        poolUpdateColumns.add(new CSVColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
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

    public static RewardDistribution toDomain(PoolReportProjection projection) {
      return RewardDistribution.builder()
          .epoch(projection.getEpochNo().toString())
          .time(projection.getTimestamp())
          .operatorReward(new BigDecimal(projection.getOperatorReward()))
          .rewardAccount(projection.getRewardAccount())
          .build();
    }

    public static List<CSVColumn> designFile() {
      List<CSVColumn> rewardDistributionColumns = new ArrayList<>();
      rewardDistributionColumns.add(
          new CSVColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
      rewardDistributionColumns.add(
          new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.DATE_TITLE));
      rewardDistributionColumns.add(new CSVColumn(ColumnFieldEnum.OPERATOR_REWARD_COLUMN,
          ColumnTitleEnum.OPERATOR_REWARD_TITLE));
      rewardDistributionColumns.add(new CSVColumn(ColumnFieldEnum.REWARD_ACCOUNT_COLUMN,
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

    private BigDecimal adaValueFees;

    private BigDecimal adaValue;

    private String owner;

    public static Deregistration toDomain(PoolReportProjection projection) {
      Deregistration result = Deregistration.builder()
          .txHash(projection.getTxnHash())
          .time(projection.getTimestamp())
          .adaValueHold(new BigDecimal(projection.getAdaValueHold()))
          .adaValueFees(new BigDecimal(projection.getAdaValueFees()))
          .owner(projection.getOwner())
          .build();
      result.setAdaValue(result.getAdaValueHold().subtract(result.getAdaValueFees()));
      return result;
    }

    public static List<CSVColumn> designFile(boolean isFee) {
      List<CSVColumn> deregistrationColumns = new ArrayList<>();
      deregistrationColumns.add(
          new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
      deregistrationColumns.add(
          new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.DATE_TITLE));
      deregistrationColumns.add(
          new CSVColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE));
      if (isFee) {
        deregistrationColumns.add(new CSVColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
            ColumnTitleEnum.ADA_VALUE_FEE_TITLE));
      }
      deregistrationColumns.add(
          new CSVColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE));
      return deregistrationColumns;
    }
  }

}
