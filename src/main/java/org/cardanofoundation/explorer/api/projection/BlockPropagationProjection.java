package org.cardanofoundation.explorer.api.projection;

import java.time.LocalDate;

public interface BlockPropagationProjection {
  LocalDate getTime();

  Integer getEpochNo();

  Integer getBlockPropMean();

  Integer getBlockPropMedian();

  Integer getBlockPropP90();

  Integer getBlockPropP95();
}
