package org.cardanofoundation.explorer.api.config;

import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.conversions.ClasspathConversionsFactory;
import org.cardanofoundation.conversions.domain.NetworkType;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;

@Configuration
@Log4j2
public class CardanoConvertersConfig {
  @Bean
  public CardanoConverters cardanoConverters(@Value("${application.network}") String network) {
    return switch (network) {
      case CommonConstant.NetworkType.PREPROD -> ClasspathConversionsFactory.createConverters(
          NetworkType.PREPROD);
      case CommonConstant.NetworkType.PREVIEW -> ClasspathConversionsFactory.createConverters(
          NetworkType.PREVIEW);
      case CommonConstant.NetworkType.SANCHONET -> ClasspathConversionsFactory.createConverters(
          NetworkType.SANCHONET);
      default -> ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    };
  }
}
