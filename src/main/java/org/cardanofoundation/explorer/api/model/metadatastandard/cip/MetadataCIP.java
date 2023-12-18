package org.cardanofoundation.explorer.api.model.metadatastandard.cip;

import java.util.Map;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MetadataCIP {

  private Map<Object, TokenCIP> tokenMap;

  private Boolean valid;
}
