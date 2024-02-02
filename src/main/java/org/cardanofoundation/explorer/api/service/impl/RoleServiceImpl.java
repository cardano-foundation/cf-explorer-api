package org.cardanofoundation.explorer.api.service.impl;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.REPORT_LIMIT_PER_24HOURS;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.UNLIMITED_REPORT;

import java.util.Map;
import java.util.Objects;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.service.RoleService;

@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {

  public int getReportLimit(Map<String, Map<String, Object>> roleDescriptions) {
    int maxReportLimit = 0;
    if (Objects.nonNull(roleDescriptions)) {
      for (String role : roleDescriptions.keySet()) {
        Map<String, Object> descriptions = roleDescriptions.get(role);
        if (Objects.nonNull(descriptions) && descriptions.containsKey(REPORT_LIMIT_PER_24HOURS)) {
          int reportLimit = (int) descriptions.get(REPORT_LIMIT_PER_24HOURS);
          if (reportLimit == UNLIMITED_REPORT) { // if report limit is equal -1 then
            return UNLIMITED_REPORT;
          }
          maxReportLimit = Math.max(maxReportLimit, reportLimit);
        }
      }
    }
    return maxReportLimit;
  }
}
