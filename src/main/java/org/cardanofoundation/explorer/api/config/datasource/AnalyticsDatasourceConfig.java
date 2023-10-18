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
    entityManagerFactoryRef = "analyticsEntityManagerFactory",
    transactionManagerRef = "analyticsTransactionManager",
    basePackages = {"org.cardanofoundation.explorer.api.repository.analytics"})
public class AnalyticsDatasourceConfig {

  private final MultiDataSourceProperties multiDataSourceProperties;

  public AnalyticsDatasourceConfig(
      MultiDataSourceProperties multiDataSourceProperties) {this.multiDataSourceProperties = multiDataSourceProperties;}

  @Bean(name = "analyticsDataSource")
  public DataSource ledgerSyncDataSource() {
    return multiDataSourceProperties.buildDataSource(
        multiDataSourceProperties.getDatasourceAnalytics());
  }

  @Bean(name = "analyticsEntityManagerFactory")
  public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
      (EntityManagerFactoryBuilder builder,
       @Qualifier("analyticsDataSource") DataSource dataSource) {
    return builder
        .dataSource(dataSource)
        .packages("org.cardanofoundation.explorer.consumercommon.analytics.entity")
        .build();
  }

  @Bean(name = "analyticsTransactionManager")
  public PlatformTransactionManager todosTransactionManager(
      @Qualifier("analyticsEntityManagerFactory") LocalContainerEntityManagerFactoryBean ledgerSyncEntityManagerFactory) {
    return new JpaTransactionManager(
        Objects.requireNonNull(ledgerSyncEntityManagerFactory.getObject()));
  }
}
