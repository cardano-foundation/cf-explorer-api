package org.cardanofoundation.explorer.api.model.metadatastandard.cip25;

import java.util.Map;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MetadataCIP25 {

  private Map<Object, TokenCIP25> tokenMap;

  private Boolean valid;
}
