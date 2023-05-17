package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.common.report.ColumnFieldEnum;
import org.cardanofoundation.explorer.common.report.ColumnTitleEnum;
import org.cardanofoundation.explorer.common.report.ExportColumn;
import org.cardanofoundation.explorer.common.report.ExportColumn.Alignment;

@Getter
@Setter
@Builder
public class StakeRegistrationLifeCycle {

  private String txHash;
  private BigInteger fee;
  private Long deposit;
  private LocalDateTime time;

  public static List<ExportColumn> buildExportColumn() {
    List<ExportColumn> columns = new ArrayList<>();
    columns.add(new ExportColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE,
                                 Alignment.LEFT));
    columns.add(new ExportColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE,
        Alignment.CENTER));
    columns.add(new ExportColumn(ColumnFieldEnum.DEPOSIT_COLUMN, ColumnTitleEnum.DEPOSIT_TITLE,
        Alignment.RIGHT));
    columns.add(
        new ExportColumn(ColumnFieldEnum.FEE_COLUMN, ColumnTitleEnum.FEES_TITLE, Alignment.RIGHT));
    return columns;
  }
}
