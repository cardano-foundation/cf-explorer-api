package org.cardanofoundation.explorer.api.model.response.tx;

import java.math.BigInteger;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonInclude;

import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRelayResponse;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class TxPoolCertificate {
  private String poolId;
  private Integer epoch;
  private String vrfKey;
  private String rewardAccount;
  private List<String> poolOwners;
  private String metadataHash;
  private String metadataUrl;
  private Double margin;
  private BigInteger cost;
  private BigInteger pledge;
  private List<PoolRelayResponse> relays;
  private CertificateType type;
}
