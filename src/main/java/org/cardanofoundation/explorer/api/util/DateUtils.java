package org.cardanofoundation.explorer.api.util;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.TemporalAmount;
import java.util.ArrayList;
import java.util.List;

import lombok.experimental.UtilityClass;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;

@UtilityClass
public class DateUtils {

  private final TemporalAmount ONE_DAY_TIME_SEGMENT = Duration.ofHours(2);
  private final TemporalAmount ONE_WEEK_TIME_SEGMENT = Duration.ofDays(1);
  private final TemporalAmount ONE_MONTH_TIME_SEGMENT = Duration.ofDays(1);
  private final TemporalAmount THREE_MONTH_TIME_SEGMENT = Duration.ofDays(1);

  private final TemporalAmount ONE_DAY = Duration.ofDays(1);
  private final TemporalAmount ONE_WEEK = Duration.ofDays(7);
  private final TemporalAmount ONE_MONTH = Duration.ofDays(30);
  private final TemporalAmount THREE_MONTH = Duration.ofDays(90);

  public static List<LocalDateTime> getListDateAnalytic(AnalyticType analyticType) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    LocalDateTime startOfToday = currentDate.atStartOfDay();
    return switch (analyticType) {
      case ONE_WEEK ->
          getDateAnalytics(startOfToday.minus(ONE_WEEK), currentDate.atStartOfDay(), ONE_WEEK_TIME_SEGMENT);
      case ONE_MONTH ->
          getDateAnalytics(startOfToday.minus(ONE_MONTH), currentDate.atStartOfDay(), ONE_MONTH_TIME_SEGMENT);
      case THREE_MONTH ->
          getDateAnalytics(startOfToday.minus(THREE_MONTH), currentDate.atStartOfDay(), THREE_MONTH_TIME_SEGMENT);
      default ->
          getDateAnalytics(startOfToday.minus(ONE_DAY), currentDate.atStartOfDay(), ONE_DAY_TIME_SEGMENT);
    };
  }

  private List<LocalDateTime> getDateAnalytics(LocalDateTime start,
                                               LocalDateTime end,
                                               TemporalAmount amountToSubtract) {
    List<LocalDateTime> dateAnalytics = new ArrayList<>();
    while (!start.isAfter(end)) {
      dateAnalytics.add(start);
      start = start.plus(amountToSubtract);
    }
    return dateAnalytics;
  }

}
