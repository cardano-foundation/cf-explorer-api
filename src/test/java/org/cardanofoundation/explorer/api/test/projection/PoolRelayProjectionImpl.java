package org.cardanofoundation.explorer.api.test.projection;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRelayProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class PoolRelayProjectionImpl implements PoolRelayProjection {
  Long poolUpdateId;
  String dnsName;
  String dnsSrvName;
  String ipv4;
  String ipv6;
  Integer port;

  @Override
  public Long getPoolUpdateId() {
    return this.poolUpdateId;
  }

  @Override
  public String getDnsName() {
    return this.dnsName;
  }

  @Override
  public String getDnsSrvName() {
    return this.dnsSrvName;
  }

  @Override
  public String getIpv4() {
    return this.ipv4;
  }

  @Override
  public String getIpv6() {
    return this.ipv6;
  }

  @Override
  public Integer getPort() {
    return this.port;
  }
}
