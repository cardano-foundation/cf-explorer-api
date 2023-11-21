package org.cardanofoundation.explorer.api.model.metadatastandard.cip25;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.ledgersync.common.model.BaseProperty;

@Getter
@Setter
public class TokenCIP25 {

  private Object tokenName;

  private List<BaseProperty> requireProperties;

  private List<BaseProperty> optionalProperties;
}
