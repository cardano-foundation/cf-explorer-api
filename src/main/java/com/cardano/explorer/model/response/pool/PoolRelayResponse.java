package com.cardano.explorer.model.response.pool;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PoolRelayResponse {
  private String dnsName;
  private String dnsSrvName;
  private String ipv4;
  private String ipv6;
  private Integer port;
}
