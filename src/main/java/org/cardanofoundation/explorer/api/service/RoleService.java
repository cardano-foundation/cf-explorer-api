package org.cardanofoundation.explorer.api.service;

import java.util.Map;

public interface RoleService {
  int getReportLimit(Map<String, Map<String, Object>> roleDescriptions);
}
