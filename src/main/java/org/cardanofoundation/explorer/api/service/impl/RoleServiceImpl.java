package org.cardanofoundation.explorer.api.service.impl;

import java.util.Map;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.service.RoleService;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.REPORT_LIMIT_PER_24HOURS;


@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {

  public int getReportLimit(Map<String, Map<String, Object>> roleDescriptions){
    int maxReportLimit = 0;
    for(String role : roleDescriptions.keySet()){
      Map<String,Object> descriptions = roleDescriptions.get(role);
      if(descriptions.containsKey(REPORT_LIMIT_PER_24HOURS)){
        int reportLimit = (int) descriptions.get(REPORT_LIMIT_PER_24HOURS);
        if(reportLimit < 0){ // if report limit is equal -1 then
          return -1;
        }
        maxReportLimit = Math.max(maxReportLimit,reportLimit);
      }
    }
    return maxReportLimit;
  }
}
