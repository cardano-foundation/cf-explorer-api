package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.common.model.ReportMessage;

public interface KafkaService {

  /**
   * Send report history to kafka
   *
   * @param reportMessage
   * @return true if send success, false if send failure
   */
  Boolean sendReportHistory(ReportMessage reportMessage);
}
