package com.cardano.explorer.model.request.pool.lifecycle;

import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolUpdateRequest {

  private String txHash;

  private String poolView;

  private Timestamp fromDate;

  private Timestamp toDate;
}
