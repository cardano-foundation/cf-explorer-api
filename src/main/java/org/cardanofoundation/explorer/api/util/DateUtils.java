package org.cardanofoundation.explorer.api.util;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.TemporalAmount;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.experimental.UtilityClass;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;

@UtilityClass
public class DateUtils {

  private final TemporalAmount AMOUNT_TO_SUBTRACT_ONE_DAY = Duration.ofHours(2);
  private final TemporalAmount AMOUNT_TO_SUBTRACT_ONE_WEEK = Duration.ofDays(1);
  private final TemporalAmount AMOUNT_TO_SUBTRACT_ONE_MONTH = Duration.ofDays(1);
  private final TemporalAmount AMOUNT_TO_SUBTRACT_THREE_MONTH = Duration.ofDays(1);

  public static List<LocalDateTime> getListDateAnalytic(AnalyticType analyticType) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    LocalDateTime startOfToday = currentDate.atStartOfDay();
    return switch (analyticType) {
      case ONE_WEEK ->
          getDateAnalytics(startOfToday.minusWeeks(1), currentDate.atStartOfDay(), AMOUNT_TO_SUBTRACT_ONE_WEEK);
      case ONE_MONTH ->
          getDateAnalytics(startOfToday.minusMonths(1), currentDate.atStartOfDay(), AMOUNT_TO_SUBTRACT_ONE_MONTH);
      case THREE_MONTH ->
          getDateAnalytics(startOfToday.minusMonths(3), currentDate.atStartOfDay(), AMOUNT_TO_SUBTRACT_THREE_MONTH);
      default ->
          getDateAnalytics(startOfToday.minusDays(1), currentDate.atStartOfDay(), AMOUNT_TO_SUBTRACT_ONE_DAY);
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
