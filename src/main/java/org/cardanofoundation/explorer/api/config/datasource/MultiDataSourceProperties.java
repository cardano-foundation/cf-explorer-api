package org.cardanofoundation.explorer.api.config.datasource;

import javax.sql.DataSource;

import lombok.AccessLevel;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.FieldDefaults;

import org.springframework.boot.autoconfigure.flyway.FlywayProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.flywaydb.core.Flyway;


@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@Configuration
@ConfigurationProperties(prefix = "multi-datasource")
public class MultiDataSourceProperties {

  DataSourceConfig datasourceLedgerSync;
  DataSourceConfig datasourceExplorer;

  @Data
  @FieldDefaults(level = AccessLevel.PRIVATE)
  public static class DataSourceConfig {
    HikariConfig hikariConfig;
    FlywayProperties flyway;
  }

  public DataSource buildDataSource(DataSourceConfig dataSourceConfig) {
    HikariDataSource hikariDataSource = new HikariDataSource(dataSourceConfig.getHikariConfig());
    FlywayProperties flywayProperties = dataSourceConfig.getFlyway();
    if (flywayProperties != null && flywayProperties.isEnabled()) {
      Flyway.configure()
          .dataSource(hikariDataSource)
          .locations(flywayProperties.getLocations().toArray(new String[0]))
          .sqlMigrationPrefix(flywayProperties.getSqlMigrationPrefix())
          .sqlMigrationSeparator(flywayProperties.getSqlMigrationSeparator())
          .sqlMigrationSuffixes(flywayProperties.getSqlMigrationSuffixes().toArray(new String[0]))
          .group(flywayProperties.isGroup())
          .defaultSchema(flywayProperties.getDefaultSchema())
          .schemas(flywayProperties.getSchemas().toArray(new String[0]))
          .validateOnMigrate(flywayProperties.isValidateOnMigrate())
          .outOfOrder(flywayProperties.isOutOfOrder())
          .load()
          .migrate();
    }
    return hikariDataSource;
  }
}
