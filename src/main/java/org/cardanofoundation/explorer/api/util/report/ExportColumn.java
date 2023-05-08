package org.cardanofoundation.explorer.api.util.report;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ExportColumn {

  // export common
  private ColumnFieldEnum columnField;
  private ColumnTitleEnum columnTitle;

  // use for export excel
  private Alignment align;
  private Integer columnWidth;

  public enum Alignment {
    LEFT, RIGHT, CENTER
  }

  public ExportColumn(ColumnFieldEnum column, ColumnTitleEnum title, Alignment align) {
    this.columnField = column;
    this.columnTitle = title;
    this.align = align;
    this.columnWidth = CommonConstant.COLUMN_WITH * 30;
  }

  /**
   * @param column
   * @param title
   */
  public ExportColumn(ColumnFieldEnum column, ColumnTitleEnum title){
    this.columnField = column;
    this.columnTitle = title;
    this.align = Alignment.LEFT;
    this.columnWidth = CommonConstant.COLUMN_WITH * 30;
  }

  public ExportColumn(ColumnFieldEnum column, ColumnTitleEnum title, Alignment align, Integer columnWidth) {
    this.columnField = column;
    this.columnTitle = title;
    this.align = align;
    this.columnWidth = CommonConstant.COLUMN_WITH * columnWidth;
  }
}
