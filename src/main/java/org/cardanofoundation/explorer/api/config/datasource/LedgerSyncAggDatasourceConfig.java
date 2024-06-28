package org.cardanofoundation.explorer.api.config.datasource;

import java.util.Objects;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
    entityManagerFactoryRef = "ledgerSyncAggEntityManagerFactory",
    transactionManagerRef = "ledgerSyncAggTransactionManager",
    basePackages = {"org.cardanofoundation.explorer.api.repository.ledgersyncagg"})
public class LedgerSyncAggDatasourceConfig {

  private final MultiDataSourceProperties multiDataSourceProperties;

  public LedgerSyncAggDatasourceConfig(MultiDataSourceProperties multiDataSourceProperties) {
    this.multiDataSourceProperties = multiDataSourceProperties;
  }

  @Bean(name = "ledgerSyncAggDataSource")
  public DataSource ledgerSyncAggDataSource() {
    return multiDataSourceProperties.buildDataSource(
        multiDataSourceProperties.getDatasourceLedgerSyncAgg());
  }

  @Bean(name = "ledgerSyncAggEntityManagerFactory")
  public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean(
      EntityManagerFactoryBuilder builder,
      @Qualifier("ledgerSyncAggDataSource") DataSource dataSource) {
    return builder
        .dataSource(dataSource)
        .packages(
            "org.cardanofoundation.explorer.common.entity.ledgersyncsagg",
            "org.cardanofoundation.explorer.common.entity.enumeration",
            "org.cardanofoundation.explorer.common.entity.validation")
        .build();
  }

  @Bean(name = "ledgerSyncAggTransactionManager")
  public PlatformTransactionManager ledgerSyncAggTransactionManager(
      @Qualifier("ledgerSyncAggEntityManagerFactory")
          LocalContainerEntityManagerFactoryBean ledgerSyncAggEntityManagerFactory) {
    return new JpaTransactionManager(
        Objects.requireNonNull(ledgerSyncAggEntityManagerFactory.getObject()));
  }
}
