package org.cardanofoundation.explorer.api.config.datasource;

import lombok.AccessLevel;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.FieldDefaults;

import org.springframework.boot.autoconfigure.flyway.FlywayProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import com.zaxxer.hikari.HikariConfig;


@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@Configuration
@ConfigurationProperties(prefix = "multi-datasource")
public class MultiDataSourceProperties {

  DataSourceConfig datasourceLedgerSync;
  DataSourceConfig datasourceAnalytics;

  @Data
  @FieldDefaults(level = AccessLevel.PRIVATE)
  public static class DataSourceConfig {
    HikariConfig hikariConfig;
    FlywayProperties flyway;
  }

}
