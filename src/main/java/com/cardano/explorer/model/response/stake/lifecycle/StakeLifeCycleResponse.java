package com.cardano.explorer.model.response.stake.lifecycle;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeLifeCycleResponse {

  private int index;
  private StakeRegistrationLifeCycle registration;
  private StakeRegistrationLifeCycle deRegistration;

}
