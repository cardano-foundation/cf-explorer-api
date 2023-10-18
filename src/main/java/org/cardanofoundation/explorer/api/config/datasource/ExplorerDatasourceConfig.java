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
    entityManagerFactoryRef = "explorerEntityManagerFactory",
    transactionManagerRef = "explorerTransactionManager",
    basePackages = {"org.cardanofoundation.explorer.api.repository.explorer"})
public class ExplorerDatasourceConfig {

  private final MultiDataSourceProperties multiDataSourceProperties;

  public ExplorerDatasourceConfig(
      MultiDataSourceProperties multiDataSourceProperties) {this.multiDataSourceProperties = multiDataSourceProperties;}

  @Bean(name = "explorerDataSource")
  public DataSource ledgerSyncDataSource() {
    return multiDataSourceProperties.buildDataSource(
        multiDataSourceProperties.getDatasourceExplorer());
  }

  @Bean(name = "explorerEntityManagerFactory")
  public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
      (EntityManagerFactoryBuilder builder,
       @Qualifier("explorerDataSource") DataSource dataSource) {
    return builder
        .dataSource(dataSource)
        .packages("org.cardanofoundation.explorer.consumercommon.explorer.entity")
        .build();
  }

  @Bean(name = "explorerTransactionManager")
  public PlatformTransactionManager todosTransactionManager(
      @Qualifier("explorerEntityManagerFactory") LocalContainerEntityManagerFactoryBean ledgerSyncEntityManagerFactory) {
    return new JpaTransactionManager(
        Objects.requireNonNull(ledgerSyncEntityManagerFactory.getObject()));
  }
}
