package com.cardano.explorer.util;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class TimeUtil {

  public static final String DATE_FORMAT_DEFAULT = "dd/MM/yyyy";

  private TimeUtil() {

  }

  @SneakyThrows
  public static Date formatDate(Timestamp timestamp) {
    return formatDate(timestamp, null);
  }

  @SneakyThrows
  public static Date formatDate(Timestamp timestamp, String pattern) {
    DateFormat formatter = new SimpleDateFormat(
        Objects.isNull(pattern) ? DATE_FORMAT_DEFAULT : pattern);
    return formatter.parse(formatter.format(new Date(timestamp.getTime())));
  }
}
