package org.cardanofoundation.explorer.api.config.datasource;

import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.springframework.boot.autoconfigure.flyway.FlywayProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import com.zaxxer.hikari.HikariDataSource;
import org.cardanofoundation.explorer.api.config.datasource.MultiDataSourceProperties.DataSourceConfig;
import org.flywaydb.core.Flyway;

@Configuration
public class DynamicDatabaseRouter {

  private final MultiDataSourceProperties multiDataSourceProperties;

  public DynamicDatabaseRouter(MultiDataSourceProperties multiDataSourceProperties) {
    this.multiDataSourceProperties = multiDataSourceProperties;
  }

  @Bean
  @Primary
  public DataSource dataSource() {
    Map<Object, Object> targetDataSources = getTargetDataSources();
    return new AbstractRoutingDataSourceImpl(
        (DataSource) targetDataSources.get(DataBaseType.LEDGER_SYNC),
        targetDataSources);
  }

  private Map<Object, Object> getTargetDataSources() {
    Map<Object, Object> targetDataSourceMap = new HashMap<>();
    targetDataSourceMap.put(DataBaseType.LEDGER_SYNC,
                            buildDataSource(multiDataSourceProperties.getDatasourceLedgerSync()));
    targetDataSourceMap.put(DataBaseType.ANALYTICS,
                            buildDataSource(multiDataSourceProperties.getDatasourceAnalytics()));
    return targetDataSourceMap;
  }

  private DataSource buildDataSource(DataSourceConfig dataSourceConfig) {
    HikariDataSource hikariDataSource = new HikariDataSource(dataSourceConfig.getHikariConfig());
    FlywayProperties flywayProperties = dataSourceConfig.getFlyway();
    if (flywayProperties.isEnabled()) {
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
