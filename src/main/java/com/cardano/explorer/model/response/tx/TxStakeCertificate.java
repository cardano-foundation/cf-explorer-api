package com.cardano.explorer.model.response.tx;

import com.cardano.explorer.common.enumeration.CertificateType;
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
