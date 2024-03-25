package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import com.fasterxml.jackson.databind.JsonNode;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

public interface GovActionDetailsProjection {
  String getTxHash();

  Integer getIndex();

  GovActionType getType();

  String getAnchorHash();

  String getAnchorUrl();

  JsonNode getDetails();

  Vote getVote();

  BigInteger getSlot();

  Long getBlockTime();

  Integer getEpoch();
}
