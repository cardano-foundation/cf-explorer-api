package org.cardanofoundation.explorer.api.util.report;


import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.ReflectorUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

@Log4j2
public class CSVHelper {

  public static final String DATE_TIME_PATTERN = "yyyy/MM/dd HH:mm:ss";
  public static final String NO_RECORDS = "No records";

  public static ByteArrayInputStream writeContent(List<ExportContent> exportContents) {
    try {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      final CSVPrinter printer = new CSVPrinter(new PrintWriter(outputStream), CSVFormat.DEFAULT);
      for (ExportContent exportContent : exportContents) {
        List<String> columnHeaders;
        // print title
        if (!DataUtil.isNullOrEmpty(exportContent.getHeaderTitle())) {
          printer.printRecord(exportContent.getHeaderTitle());
        }

        // print column header
        if (!DataUtil.isNullOrEmpty(exportContent.getLstColumn())) {
          columnHeaders = exportContent.getLstColumn().stream()
              .map(exportColumn -> exportColumn.getColumnTitle().getValue())
              .collect(Collectors.toList());
          printer.printRecord(columnHeaders);

          if (DataUtil.isNullOrEmpty(exportContent.getLstData())) {
            printer.printRecord(NO_RECORDS);
            continue;
          }
        }

        if (DataUtil.isNullOrEmpty(exportContent.getClazz()) ||
            DataUtil.isNullOrEmpty(exportContent.getLstColumn())) {
          continue;
        }

        List<Field> fields = ReflectorUtil.getAllFields(exportContent.getClazz());
        Map<String, Field> mapField = new HashMap<>();

        exportContent.getLstColumn()
            .forEach(exportColumn -> fields.stream()
                .peek(f -> f.setAccessible(true))
                .filter(f -> f.getName().equals(exportColumn.getColumnField().getValue()))
                .forEach(f -> mapField.put(exportColumn.getColumnField().getValue(), f)));

        for (Object obj : exportContent.getLstData()) {
          List<String> record = new ArrayList<>();
          for (ExportColumn exportColumn : exportContent.getLstColumn()) {
            Field field = mapField.get(exportColumn.getColumnField().getValue());
            if (field == null) {
              log.error("Field not found: " + exportColumn.getColumnField().getValue());
              throw new BusinessException(BusinessCode.INTERNAL_ERROR);
            }
            Object value = field.get(obj);
            String text;
            if (value instanceof Double) {
              text = DataUtil.doubleToString((Double) value);
            } else if (value instanceof Instant) {
              text = DataUtil.instantToString((Instant) value, DATE_TIME_PATTERN);
            } else if (value instanceof Timestamp) {
              text = DataUtil.instantToString(((Timestamp) value).toInstant(), DATE_TIME_PATTERN);
            } else if (value instanceof Date) {
              text = DataUtil.instantToString(((Date) value).toInstant(), DATE_TIME_PATTERN);
            } else if (value instanceof LocalDateTime) {
              text = DataUtil.instantToString(
                  ((LocalDateTime) value).atZone(ZoneId.systemDefault()).toInstant(),
                  DATE_TIME_PATTERN);
            } else {
              text = DataUtil.objectToString(value);
            }
            record.add(text);
          }
          printer.printRecord(record);
        }
        printer.printRecord();
      }
      printer.flush();
      return new ByteArrayInputStream(outputStream.toByteArray());
    } catch (final IOException | IllegalAccessException e) {
      log.error("Csv writing error: ", e);
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }
}
