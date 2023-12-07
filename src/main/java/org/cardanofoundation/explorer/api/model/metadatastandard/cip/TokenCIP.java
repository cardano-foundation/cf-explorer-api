package org.cardanofoundation.explorer.api.model.metadatastandard.cip;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.model.metadatastandard.BaseProperty;

@Getter
@Setter
public class TokenCIP {

  private Object tokenName;

  private List<BaseProperty> requireProperties;

  private List<BaseProperty> optionalProperties;
}
