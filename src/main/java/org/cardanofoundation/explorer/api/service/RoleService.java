package org.cardanofoundation.explorer.api.service;

import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

public interface RoleService {
  int getReportLimit(Map<String, Map<String, Object>> roleDescriptions);

}
