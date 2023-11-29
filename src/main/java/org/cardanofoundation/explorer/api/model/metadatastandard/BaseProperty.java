package org.cardanofoundation.explorer.api.model.metadatastandard;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class BaseProperty {

  private String index;

  private Boolean valid;

  private Object value;

  private String property;

  private String format;
}
