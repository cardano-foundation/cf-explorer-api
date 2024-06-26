package org.cardanofoundation.explorer.api.model.response.protocol;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EpochChange implements Cloneable {

  Integer startEpoch;
  Integer endEpoch;

  @Override
  public EpochChange clone() {
    return EpochChange.builder().startEpoch(this.startEpoch).endEpoch(this.endEpoch).build();
  }
}
