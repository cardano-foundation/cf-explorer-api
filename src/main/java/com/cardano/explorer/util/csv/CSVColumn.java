package com.cardano.explorer.util.csv;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class CSVColumn {

  private ColumnFieldEnum columnField;
  private ColumnTitleEnum columnTitle;
}
