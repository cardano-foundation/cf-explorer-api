package org.cardanofoundation.explorer.api.model.response.tx;

import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TxStakeCertificate {
  private String stakeAddress;
  private CertificateType type;
}
