package org.cardanofoundation.explorer.api.config.datasource;

import java.util.Map;

import javax.sql.DataSource;

import org.springframework.jdbc.datasource.lookup.AbstractRoutingDataSource;


public class AbstractRoutingDataSourceImpl extends AbstractRoutingDataSource {

  private static final ThreadLocal<DataBaseType> ROUTE_CONTEXT = new ThreadLocal<>();

  public AbstractRoutingDataSourceImpl(DataSource defaultTargetDatasource,
                                       Map<Object, Object> targetDatasources) {
    this.setDefaultTargetDataSource(defaultTargetDatasource);
    this.setTargetDataSources(targetDatasources);
    this.afterPropertiesSet();
  }

  public static DataBaseType getRoute() {
    return ROUTE_CONTEXT.get();
  }

  public static void setRoute(DataBaseType dataBaseType) {ROUTE_CONTEXT.set(dataBaseType);}

  public static void clearRoute() {
    ROUTE_CONTEXT.remove();
  }

  @Override
  protected Object determineCurrentLookupKey() {
    return ROUTE_CONTEXT.get();
  }
}
