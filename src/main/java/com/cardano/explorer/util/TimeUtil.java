package com.cardano.explorer.util;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class TimeUtil {

  private TimeUtil() {

  }

  @SneakyThrows
  public static Date formatDate(Timestamp timestamp) {
    return formatDate(timestamp,"dd/MM/yyyy");
  }
  @SneakyThrows
  public static Date formatDate(Timestamp timestamp, String pattern) {
    DateFormat formatter = new SimpleDateFormat(pattern);
    return formatter.parse(formatter.format(new Date(timestamp.getTime())));
  }
}
