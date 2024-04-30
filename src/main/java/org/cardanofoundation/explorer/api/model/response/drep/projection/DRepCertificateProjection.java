package org.cardanofoundation.explorer.api.model.response.drep.projection;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.DRepActionType;

public interface DRepCertificateProjection {

  String getTxHash();

  Long getTxIndex();

  DRepActionType getType();

  Long getBlockNo();

  Long getSlotNo();

  Integer getEpochNo();

  Long getBlockTime();

  Long getAbsoluteSlot();
}
