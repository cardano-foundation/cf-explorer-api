package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class RegistrationAllResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private BigInteger poolHold;

  private List<RegistrationResponse> registrations;
}
