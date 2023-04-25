package com.cardano.explorer.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

@Slf4j
public class CSVHelper {

  public static final String DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss";


  public static <T> ByteArrayInputStream exportCSV(List<T> data, List<CSVColumn> csvColumnList) {
    return exportCSV(data, csvColumnList, null);
  }

  public static <T> ByteArrayInputStream exportCSV(List<T> data, List<CSVColumn> csvColumnList, byte[] prefixBytes) {
    try {
      String[] columnHeaders = csvColumnList.stream().map(CSVColumn::getTitle).toArray(String[]::new);
      CSVFormat csvFormat = CSVFormat.DEFAULT.withHeader(columnHeaders);
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

      if(prefixBytes != null && prefixBytes.length > 0) {
        outputStream = new ByteArrayOutputStream(prefixBytes.length);
        outputStream.write(prefixBytes, 0, prefixBytes.length);
      }

      final CSVPrinter printer = new CSVPrinter(new PrintWriter(outputStream), csvFormat);
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
            } else if (value instanceof Date) {
              text = DataUtil.dateToString((Date) value, DATE_TIME_PATTERN);
            } else {
              text = DataUtil.objectToString(value);
            }
            record.add(text);
          } catch (IllegalAccessException ex) {
            throw new RuntimeException("Can't parse field: " + csvColumn.getColumn() + " " + ex.getMessage());
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
