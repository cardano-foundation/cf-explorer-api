package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

public interface GovernanceActionOverviewProjection {
  String getTxHash();

  Integer getIndex();

  Long getDateCreated();

  GovActionType getActionType();

  GovActionStatus getStatus();

  String getAbstract();

  String getMotivation();

  String getRationale();

  String getRawData();

  String getAnchorHash();

  String getAnchorUrl();
}
