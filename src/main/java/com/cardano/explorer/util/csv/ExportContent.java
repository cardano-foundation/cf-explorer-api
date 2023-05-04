package com.cardano.explorer.util.csv;

import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class ExportContent {
  Class<?> clazz;
  List<?> lstData;
  List<CSVColumn> lstColumn;
  String headerTitle;
}
