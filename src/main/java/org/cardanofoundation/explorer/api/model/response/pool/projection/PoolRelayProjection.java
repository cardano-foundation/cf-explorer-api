package org.cardanofoundation.explorer.api.model.response.pool.projection;

public interface PoolRelayProjection {

  Long getPoolUpdateId();
  String getDnsName();
  String getDnsSrvName();
  String getIpv4();
  String getIpv6();
  Integer getPort();

}
