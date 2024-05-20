package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import com.fasterxml.jackson.databind.JsonNode;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

public interface GovActionDetailsProjection {
  String getTxHash();

  Integer getIndex();

  GovActionType getType();

  String getAnchorHash();

  String getAnchorUrl();

  JsonNode getDetails();

  Long getBlockTime();

  Long getSlot();

  Integer getEpoch();

  GovActionStatus getStatus();

  BigInteger getVotingPower();

  BigInteger getIndexType();
}
