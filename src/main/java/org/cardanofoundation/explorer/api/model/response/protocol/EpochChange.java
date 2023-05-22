package org.cardanofoundation.explorer.api.model.response.protocol;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class EpochChange {
  Integer startEpoch;
  Integer endEpoch;
}
