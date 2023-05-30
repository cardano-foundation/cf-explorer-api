package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DeRegistrationAllResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private List<String> stakeKeys;

  List<DeRegistrationResponse> deRegistrations;
}
