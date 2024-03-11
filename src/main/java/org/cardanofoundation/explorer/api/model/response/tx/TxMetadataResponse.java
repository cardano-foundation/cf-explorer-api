package org.cardanofoundation.explorer.api.model.response.tx;

import java.util.Map;

import lombok.*;

import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip.MetadataCIP;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxMetadataResponse {
  private String label;
  private String value;
  private MetadataCIP metadataCIP25;
  private MetadataCIP metadataCIP60;
  private Map<String, Object> metadataCIP20;
  private Map<String, Object> metadataCIP83;
  private MetadataBolnisi metadataBolnisi;
}
