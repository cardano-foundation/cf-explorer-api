package org.cardanofoundation.explorer.api.config.kafka;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import org.springframework.kafka.support.serializer.JsonSerializer;

import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;

@Configuration
@EnableConfigurationProperties(KafkaTopic.class)
public class KafKaProducerConfiguration {

  private static final int MAX_REQUEST_SIZE = 2_097_152;
  @Value("${spring.kafka.bootstrap-servers}")
  private String bootstrapServers;

  @Value("${spring.kafka.producer.acks}")
  private String acks;

  @Value("${spring.kafka.producer.retries}")
  private int retriesTime;

  @Value("${spring.kafka.producer.properties.enable.idempotence}")
  private boolean isIdempotence;

  @Value("${spring.kafka.producer.properties.max.in.flight.requests.per.connection}")
  private int requestPerConnection;

  @Value("${spring.kafka.producer.retryBackoff}")
  private int retryBackoff;

  @Bean
  public ProducerFactory<String, Object> producerFactory() {
    Map<String, Object> map = new HashMap<>();

    map.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG,
        bootstrapServers);
    map.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG,
        StringSerializer.class);
    map.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG,
        JsonSerializer.class);
    map.put(ProducerConfig.RETRIES_CONFIG,
        retriesTime);
    map.put(ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG,
        isIdempotence);
    map.put(ProducerConfig.MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION,
        requestPerConnection);
    map.put(ProducerConfig.RETRY_BACKOFF_MS_CONFIG,retryBackoff);
    map.put(ProducerConfig.MAX_REQUEST_SIZE_CONFIG, MAX_REQUEST_SIZE);
    map.put(ProducerConfig.ACKS_CONFIG,acks);

    return new DefaultKafkaProducerFactory<>(map);
  }

  @Bean
  KafkaTemplate<String, Object> kafkaTemplate(ProducerFactory<String, Object> producerFactory) {
    return new KafkaTemplate<>(producerFactory);
  }

}