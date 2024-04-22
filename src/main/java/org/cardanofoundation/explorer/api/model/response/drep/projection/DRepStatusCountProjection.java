package org.cardanofoundation.explorer.api.model.response.drep.projection;

import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;

public interface DRepStatusCountProjection {

  DRepStatus getStatus();

  Long getCnt();
}
