package com.cardano.explorer.util.csv;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.util.DataUtil;
import com.cardano.explorer.util.ReflectorUtil;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

@Slf4j
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
              .map(csvColumn -> csvColumn.getColumnTitle().getValue())
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
            .forEach(csvColumn -> fields.stream()
                .peek(f -> f.setAccessible(true))
                .filter(f -> f.getName().equals(csvColumn.getColumnField().getValue()))
                .forEach(f -> mapField.put(csvColumn.getColumnField().getValue(), f)));

        for (Object obj : exportContent.getLstData()) {
          List<String> record = new ArrayList<>();
          for (CSVColumn csvColumn : exportContent.getLstColumn()) {
            Field field = mapField.get(csvColumn.getColumnField().getValue());
            if (field == null) {
              log.error("Field not found: " + csvColumn.getColumnField().getValue());
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
