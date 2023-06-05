package org.cardanofoundation.explorer.api.model.response.pool.report;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiOsProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.report.ColumnFieldEnum;
import org.cardanofoundation.explorer.api.util.report.ColumnTitleEnum;
import org.cardanofoundation.explorer.api.util.report.ExportColumn;
import org.cardanofoundation.explorer.api.util.report.ExportColumn.Alignment;

import java.math.BigDecimal;
import java.math.BigInteger;
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

    private BigInteger fee;

    private BigDecimal size;

    public static EpochSize toDomain(PoolReportProjection projection) {
      return EpochSize.builder()
          .epoch(projection.getEpochNo().toString())
          .fee(projection.getFee())
          .size(new BigDecimal(projection.getSize()))
          .build();
    }

    public static EpochSize toDomain(PoolHistoryKoiOsProjection projection) {
      System.out.println(projection.getEpochNo());
      System.out.println(projection.getActiveStake());
      System.out.println(projection.getPoolFees());
      return EpochSize.builder()
          .epoch(projection.getEpochNo().toString())
          .size(new BigDecimal(projection.getActiveStake()))
          .fee(projection.getPoolFees())
          .build();
    }

    public static List<ExportColumn> designFile(Boolean isFeePaid) {
      List<ExportColumn> epochSizeColumns = new ArrayList<>();
      epochSizeColumns.add(
          new ExportColumn(ColumnFieldEnum.SIZE_COLUMN, ColumnTitleEnum.SIZE_TITLE,
                           Alignment.RIGHT));
      if (Boolean.TRUE.equals(isFeePaid)) {
        epochSizeColumns.add(
            new ExportColumn(ColumnFieldEnum.FEE_COLUMN, ColumnTitleEnum.FEES_TITLE,
                             Alignment.RIGHT));
      }
      epochSizeColumns.add(
          new ExportColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE,
                           Alignment.RIGHT));
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
      result.setAdaValue(new BigDecimal(response.getTotalFee()));
      return result;
    }

    public static List<ExportColumn> designFile() {
      List<ExportColumn> poolRegistrationsColumns = new ArrayList<>();
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE,
                           Alignment.LEFT, 61 * 255));
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE,
                           Alignment.CENTER));
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE,
                           Alignment.RIGHT));
      poolRegistrationsColumns.add(
          new ExportColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE,
                           Alignment.LEFT, 59 * 255));
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

    public static List<ExportColumn> designFile() {
      List<ExportColumn> poolUpdateColumns = new ArrayList<>();
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE,
                           Alignment.LEFT, 61 * 255));
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE,
                           Alignment.CENTER));
      poolUpdateColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE,
                           Alignment.RIGHT));
      poolUpdateColumns.add(new ExportColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
                                             ColumnTitleEnum.ADA_VALUE_FEE_TITLE,
                                             Alignment.RIGHT));
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
          new ExportColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE,
                           Alignment.RIGHT));
      rewardDistributionColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE,
                           Alignment.CENTER));
      rewardDistributionColumns.add(new ExportColumn(ColumnFieldEnum.OPERATOR_REWARD_COLUMN,
                                                     ColumnTitleEnum.OPERATOR_REWARD_TITLE,
                                                     Alignment.RIGHT));
      rewardDistributionColumns.add(new ExportColumn(ColumnFieldEnum.REWARD_ACCOUNT_COLUMN,
                                                     ColumnTitleEnum.REWARD_ACCOUNT_TITLE,
                                                     Alignment.LEFT, 59 * 255));
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
          .adaValueHold(DataUtil.isNullOrEmpty(response.getPoolHold()) ? null : new BigDecimal(
              response.getPoolHold()))
          .adaValueFee(new BigDecimal(response.getFee()))
          .owner(String.join("\n", response.getStakeKeys()))
          .build();

      result.setAdaValue(new BigDecimal(response.getTotalFee()));
      return result;
    }

    public static List<ExportColumn> designFile() {
      List<ExportColumn> deregistrationColumns = new ArrayList<>();
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE,
                           Alignment.LEFT, 61 * 255));
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.DATE_TITLE,
                           Alignment.CENTER));
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.ADA_VALUE_COLUMN, ColumnTitleEnum.ADA_VALUE_TITLE,
                           Alignment.RIGHT));
      deregistrationColumns.add(new ExportColumn(ColumnFieldEnum.ADA_VALUE_FEE_COLUMN,
                                                 ColumnTitleEnum.ADA_VALUE_FEE_TITLE,
                                                 Alignment.RIGHT));
      deregistrationColumns.add(
          new ExportColumn(ColumnFieldEnum.OWNER_COLUMN, ColumnTitleEnum.OWNER_TITLE,
                           Alignment.LEFT, 59 * 255));
      return deregistrationColumns;
    }
  }

}
