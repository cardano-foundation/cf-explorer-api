package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.*;

import java.math.BigInteger;
import org.cardanofoundation.ledgersync.common.model.cip25.MetadataCIP25;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxMetadataResponse {
  private BigInteger label;
  private String value;
  private MetadataCIP25 metadataCIP25;
}
