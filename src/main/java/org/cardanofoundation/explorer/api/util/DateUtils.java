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
  private final TemporalAmount AMOUNT_TO_SUBTRACT_ONE_MONTH = Duration.ofDays(2);
  private final TemporalAmount AMOUNT_TO_SUBTRACT_THREE_MONTH = Duration.ofDays(7);

  public static List<LocalDateTime> getListDateAnalytic(AnalyticType analyticType) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    LocalDateTime startOfToday = currentDate.atStartOfDay();
    // Default value for one day
    TemporalAmount amountToSubtract = AMOUNT_TO_SUBTRACT_ONE_DAY;
    LocalDateTime startMilestone = startOfToday.minusDays(1);
    switch (analyticType) {
      case ONE_WEEK -> {
        amountToSubtract = AMOUNT_TO_SUBTRACT_ONE_WEEK;
        startMilestone = startOfToday.minusWeeks(1);
      }
      case ONE_MONTH -> {
        amountToSubtract = AMOUNT_TO_SUBTRACT_ONE_MONTH;
        startMilestone = startOfToday.minusMonths(1);
      }
      case THREE_MONTH -> {
        amountToSubtract = AMOUNT_TO_SUBTRACT_THREE_MONTH;
        startMilestone = startOfToday.minusMonths(3);
      }
    }
    List<LocalDateTime> dateAnalytics = getDateAnalytics(startOfToday, startMilestone, amountToSubtract);
    Collections.reverse(dateAnalytics);
    return dateAnalytics;
  }

  private List<LocalDateTime> getDateAnalytics(LocalDateTime startOfToday,
                                               LocalDateTime startMilestone,
                                               TemporalAmount amountToSubtract) {
    List<LocalDateTime> dateAnalytics = new ArrayList<>();
    while (!startOfToday.isBefore(startMilestone)) {
      dateAnalytics.add(startOfToday);
      startOfToday = startOfToday.minus(amountToSubtract);
    }
    return dateAnalytics;
  }

}
