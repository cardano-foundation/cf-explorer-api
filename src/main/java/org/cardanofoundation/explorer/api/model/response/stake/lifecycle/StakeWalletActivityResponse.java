package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.util.ArrayList;
import java.util.List;

import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.common.report.ColumnFieldEnum;
import org.cardanofoundation.explorer.common.report.ColumnTitleEnum;
import org.cardanofoundation.explorer.common.report.ExportColumn;
import org.cardanofoundation.explorer.common.report.ExportColumn.Alignment;

import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;


@Getter
@Setter
public class StakeWalletActivityResponse implements Serializable {

  private String txHash;
  private BigInteger amount;
  private BigInteger fee;
  private LocalDateTime time;
  private StakeTxType type;
  private TxStatus status;

  public static List<ExportColumn> buildStakeWalletActivityColumn(Boolean isFeePaid) {
    List<ExportColumn> columns = new ArrayList<>();
    columns.add(new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE,
                                 Alignment.LEFT));
    columns.add(new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE,
                                 Alignment.CENTER));
    columns.add(new ExportColumn(ColumnFieldEnum.AMOUNT_COLUMN, ColumnTitleEnum.AMOUNT_ADA_TITLE,
                                 Alignment.RIGHT));
    if (Boolean.TRUE.equals(isFeePaid)) {
      columns.add(new ExportColumn(ColumnFieldEnum.FEE_COLUMN, ColumnTitleEnum.FEES_TITLE,
                                   Alignment.RIGHT));
    }
    columns.add(new ExportColumn(ColumnFieldEnum.TYPE_COLUMN, ColumnTitleEnum.TX_TYPE_TITLE,
                                 Alignment.CENTER));
    columns.add(new ExportColumn(ColumnFieldEnum.STATUS_COLUMN, ColumnTitleEnum.STATUS_TITLE,
                                 Alignment.CENTER));
    return columns;
  }
}
