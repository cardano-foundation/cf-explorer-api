package org.cardanofoundation.explorer.api.projection;

import com.fasterxml.jackson.databind.JsonNode;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;

public interface GovActionDetailsProjection {
  String getTxHash();

  Integer getIndex();

  GovActionType getType();

  String getAnchorHash();

  String getAnchorUrl();

  JsonNode getDetails();

  Long getBlockTime();

  Integer getEpoch();
}
