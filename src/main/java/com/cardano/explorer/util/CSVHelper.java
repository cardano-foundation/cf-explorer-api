package com.cardano.explorer.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

@Slf4j
public class CSVHelper {

  public static final String DATE_TIME_PATTERN = "yyyy/MM/dd HH:mm:ss";

  public static ByteArrayInputStream writeContent(byte[] prefixBytes, String title) {
    return writeContent(Collections.emptyList(), Collections.emptyList(), prefixBytes, title);
  }

  public static ByteArrayInputStream writeContent(String title) {
    return writeContent(Collections.emptyList(), Collections.emptyList(), null, title);
  }

  public static <T> ByteArrayInputStream writeContent(List<T> data, List<CSVColumn> csvColumnList,
      String title) {
    return writeContent(data, csvColumnList, null, title);
  }

  public static <T> ByteArrayInputStream writeContent(List<T> data, List<CSVColumn> csvColumnList,
      byte[] prefixBytes, String title) {
    try {
      List<String> columnHeaders = csvColumnList.stream().map(CSVColumn::getTitle).collect(
          Collectors.toList());
      CSVFormat csvFormat = CSVFormat.DEFAULT;
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

      if (prefixBytes != null && prefixBytes.length > 0) {
        outputStream = new ByteArrayOutputStream(prefixBytes.length);
        outputStream.write(prefixBytes, 0, prefixBytes.length);
      }

      final CSVPrinter printer = new CSVPrinter(new PrintWriter(outputStream), csvFormat);
      if (!DataUtil.isNullOrEmpty(title)) {
        printer.printRecord(title);
      }
      if (!DataUtil.isNullOrEmpty(columnHeaders)) {
        printer.printRecord(columnHeaders);
      }
      for (T item : data) {
        List<String> record = new ArrayList<>();
        for (CSVColumn csvColumn : csvColumnList) {
          try {
            Field field = ReflectorUtil.getFieldByName(item.getClass(), csvColumn.getColumn());
            if (field == null) {
              throw new RuntimeException("Field not found: " + csvColumn.getColumn());
            }
            field.setAccessible(true);
            Object value = field.get(item);
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
              text = DataUtil.instantToString(((LocalDateTime) value).atZone(
                  ZoneId.of(DataUtil.TIME_ZONE)).toInstant(), DATE_TIME_PATTERN);
            } else {
              text = DataUtil.objectToString(value);
            }
            record.add(text);
          } catch (IllegalAccessException ex) {
            throw new RuntimeException(
                "Can't parse field: " + csvColumn.getColumn() + " " + ex.getMessage());
          }
        }
        printer.printRecord(record);
      }
      printer.printRecord();
      printer.flush();
      return new ByteArrayInputStream(outputStream.toByteArray());
    } catch (final Exception e) {
      throw new RuntimeException("Csv writing error: " + e.getMessage());
    }
  }


}
