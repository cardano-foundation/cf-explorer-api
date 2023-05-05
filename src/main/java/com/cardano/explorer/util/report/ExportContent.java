package com.cardano.explorer.util.report;

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
  List<ExportColumn> lstColumn;
  String headerTitle;
}
