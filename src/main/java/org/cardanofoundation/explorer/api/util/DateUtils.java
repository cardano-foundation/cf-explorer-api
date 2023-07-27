package org.cardanofoundation.explorer.api.util;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import lombok.experimental.UtilityClass;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;

@UtilityClass
public class DateUtils {

  public static List<LocalDateTime> getListDateAnalytic(AnalyticType analyticType) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    LocalDateTime startOfYesterday = currentDate.minusDays(1).atStartOfDay();
    List<LocalDateTime> dateAnalytics = new ArrayList<>();
    switch (analyticType) {
      case ONE_DAY -> {
        LocalDateTime startMilestone = startOfYesterday;
        while (!startMilestone.isAfter(currentDate.atStartOfDay())) {
          dateAnalytics.add(startMilestone);
          startMilestone = startMilestone.plusHours(2);
        }
      }
      case ONE_WEEK -> {
        for (int inc = 7; inc >= 0; inc--) {
          dateAnalytics.add(startOfYesterday.minusDays(inc));
        }
      }
      case ONE_MONTH -> {
        LocalDateTime startMilestone = startOfYesterday.minusMonths(1).minusDays(2);
        while (!startMilestone.isAfter(startOfYesterday)) {
          dateAnalytics.add(startMilestone);
          startMilestone = startMilestone.plusDays(2);
        }
      }
      case THREE_MONTH -> {
        LocalDateTime startMilestone = startOfYesterday.minusMonths(3).minusDays(7);
        while (!startMilestone.isAfter(startOfYesterday)) {
          dateAnalytics.add(startMilestone);
          startMilestone = startMilestone.plusDays(7);
        }
      }
    }
    return dateAnalytics;
  }

//  public static void main(String[] args) {
//    List<LocalDateTime> data = getListDateAnalytic2(AnalyticType.THREE_MONTH);
//    data.forEach(System.err::println);
//  }

}
