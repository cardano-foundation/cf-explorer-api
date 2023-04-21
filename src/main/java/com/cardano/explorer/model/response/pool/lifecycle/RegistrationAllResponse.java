package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RegistrationAllResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private List<RegistrationResponse> registrations;
}
