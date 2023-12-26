package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.*;

import java.math.BigInteger;
import java.util.Map;

import org.cardanofoundation.explorer.api.model.metadatastandard.cip.MetadataCIP;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxMetadataResponse {
  private BigInteger label;
  private String value;
  private MetadataCIP metadataCIP25;
  private MetadataCIP metadataCIP60;
  private Map<String,Object> metadataCIP20;
  private Map<String,Object> metadataCIP83;
}
