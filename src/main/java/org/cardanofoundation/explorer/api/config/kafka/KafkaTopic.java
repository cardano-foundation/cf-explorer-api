package org.cardanofoundation.explorer.api.config.kafka;

import lombok.Getter;
import lombok.Setter;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Class stored kafka topic name
 */


@Getter
@Setter
@ConfigurationProperties(prefix = "spring.kafka.topics")
public class KafkaTopic {

  private String reports;
}
