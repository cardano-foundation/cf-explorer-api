package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;

public interface KafkaService {

  /**
   * Send report history to kafka
   * @param reportHistory
   * @return true if send success, false if send failure
   */
  Boolean sendReportHistory(ReportHistory reportHistory);
}
