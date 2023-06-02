package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SPOStatusResponse {

  private Boolean isRegistration;

  private Boolean isUpdate;

  private Boolean isReward;

  private Boolean isDeRegistration;
}
