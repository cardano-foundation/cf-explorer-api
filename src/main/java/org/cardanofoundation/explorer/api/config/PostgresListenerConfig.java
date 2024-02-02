package org.cardanofoundation.explorer.api.config;

import lombok.extern.slf4j.Slf4j;

import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.cardanofoundation.explorer.api.event.blocksync.BlockEventListener;
import org.cardanofoundation.explorer.api.event.blocksync.BlockNotificationHandler;

@Configuration
@Slf4j
public class PostgresListenerConfig {

  @Bean
  CommandLineRunner startListener(
      BlockNotificationHandler handler, BlockEventListener blockEventListener) {
    return (args) -> blockEventListener.listenForNotifications(handler);
  }
}
