package org.cardanofoundation.explorer.api;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.scheduling.annotation.EnableScheduling;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.conversions.ClasspathConversionsFactory;
import org.cardanofoundation.conversions.domain.NetworkType;

@SpringBootApplication
@EnableAspectJAutoProxy(proxyTargetClass = true)
// @EnableJpaAuditing
@EnableCaching
@EntityScan({"org.cardanofoundation.explorer.*", "org.cardanofoundation.*"})
@EnableScheduling
public class ExplorerApiApplication {

  public static void main(String[] args) {
    SpringApplication.run(ExplorerApiApplication.class, args);
  }

  @Bean
  public CardanoConverters cardanoConverters(@Value("${application.network}") String network) {
    return switch (network) {
      case "preprod" -> ClasspathConversionsFactory.createConverters(NetworkType.PREPROD);
      case "preview" -> ClasspathConversionsFactory.createConverters(NetworkType.PREVIEW);
      default -> ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    };
  }
}
