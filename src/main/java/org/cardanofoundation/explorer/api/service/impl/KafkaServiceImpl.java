package org.cardanofoundation.explorer.api.service.impl;

import java.util.concurrent.CompletableFuture;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.config.kafka.KafkaTopic;
import org.cardanofoundation.explorer.api.service.KafkaService;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;

@Log4j2
@Service
@RequiredArgsConstructor
public class KafkaServiceImpl implements KafkaService {

  private final KafkaTemplate<String, Object> kafkaTemplate;
  private final KafkaTopic topic;

  @Override
  public void sendReportHistory(ReportHistory reportHistory) {
    CompletableFuture<SendResult<String, Object>> future = kafkaTemplate.send(
        topic.getReports(),
        String.valueOf(reportHistory.getId()), reportHistory);
    future.whenComplete((result, ex) -> {
      if (ex != null) {
        log.debug("Send ReportHistory {} with type {} failure", reportHistory.getId(),
                  reportHistory.getType());
      } else {
        log.debug("Send ReportHistory {} with type {} successfully", reportHistory.getId(),
                  reportHistory.getType());
      }
    });
  }
}
