package org.cardanofoundation.explorer.api.config.datasource;

import java.util.Objects;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
    entityManagerFactoryRef = "ledgerSyncEntityManagerFactory",
    transactionManagerRef = "ledgerSyncTransactionManager",
    basePackages = {"org.cardanofoundation.explorer.api.repository.ledgersync"})
public class LedgerSyncDatasourceConfig {

  private final MultiDataSourceProperties multiDataSourceProperties;

  public LedgerSyncDatasourceConfig(
      MultiDataSourceProperties multiDataSourceProperties) {this.multiDataSourceProperties = multiDataSourceProperties;}

  @Primary
  @Bean(name = "ledgerSyncDataSource")
  public DataSource ledgerSyncDataSource() {
    return multiDataSourceProperties.buildDataSource(
        multiDataSourceProperties.getDatasourceLedgerSync());
  }

  @Primary
  @Bean(name = "ledgerSyncEntityManagerFactory")
  public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
      (EntityManagerFactoryBuilder builder,
       @Qualifier("ledgerSyncDataSource") DataSource dataSource) {
    return builder
        .dataSource(dataSource)
        .packages("org.cardanofoundation.explorer.consumercommon.entity")
        .build();
  }

  @Primary
  @Bean(name = "ledgerSyncTransactionManager")
  public PlatformTransactionManager todosTransactionManager(
      @Qualifier("ledgerSyncEntityManagerFactory") LocalContainerEntityManagerFactoryBean ledgerSyncEntityManagerFactory) {
    return new JpaTransactionManager(
        Objects.requireNonNull(ledgerSyncEntityManagerFactory.getObject()));
  }
}
