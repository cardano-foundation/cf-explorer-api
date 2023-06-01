package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;

public interface KafkaService {

  void sendReportHistory(ReportHistory reportHistory);
}
