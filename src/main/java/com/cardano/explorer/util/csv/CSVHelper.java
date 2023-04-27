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
import java.math.BigInteger;
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

  /**
   * Write the data to CSV file
   *
   * @param prefixBytes the prefix bytes to be written to the output stream
   * @param title       the first line data to be written to the output stream
   * @return the output stream with the prefix bytes and the title
   */
  public static ByteArrayInputStream writeContent(byte[] prefixBytes, String title) {
    return writeContent(Collections.emptyList(), Collections.emptyList(), prefixBytes, title);
  }

  /**
   * Write the data to CSV file
   *
   * @param title the first line data to be written to the output stream
   * @return the output stream with the title
   */
  public static ByteArrayInputStream writeContent(String title) {
    return writeContent(Collections.emptyList(), Collections.emptyList(), null, title);
  }

  /**
   * Write the data to CSV file
   *
   * @param data          the data to be written to the output stream
   * @param csvColumnList the column list to be written to the output stream
   * @param title         the first line data to be written to the output stream
   * @return the output stream with the data and the title
   */
  public static <T> ByteArrayInputStream writeContent(List<T> data, List<CSVColumn> csvColumnList,
      String title) {
    return writeContent(data, csvColumnList, null, title);
  }

  /**
   * Write the data to CSV file
   *
   * @param data          the data to be written to the output stream
   * @param csvColumnList the column list to be written to the output stream
   * @param prefixBytes   the prefix bytes to be written to the output stream
   * @param title         the first line data to be written to the output stream
   * @return the output stream with the data, the prefix bytes and the title
   */
  public static <T> ByteArrayInputStream writeContent(List<T> data, List<CSVColumn> csvColumnList,
      byte[] prefixBytes, String title) {
    try {
      List<String> columnHeaders = csvColumnList.stream()
          .map(csvColumn -> csvColumn.getColumnTitle().getValue())
          .collect(Collectors.toList());
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
            Field field = ReflectorUtil.getFieldByName(item.getClass(),
                csvColumn.getColumnFiled().getValue());
            if (field == null) {
              log.error("Field not found: " + csvColumn.getColumnFiled().getValue());
              throw new BusinessException(BusinessCode.INTERNAL_ERROR);
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
            }
            else {
              text = DataUtil.objectToString(value);
            }
            record.add(text);
          } catch (IllegalAccessException ex) {
            log.error("Can't parse field: " + csvColumn.getColumnFiled().getValue(), ex);
            throw new BusinessException(BusinessCode.INTERNAL_ERROR);
          }
        }
        printer.printRecord(record);
      }
      printer.printRecord();
      printer.flush();
      return new ByteArrayInputStream(outputStream.toByteArray());
    } catch (final IOException e) {
      log.error("Csv writing error: ", e);
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }


}
