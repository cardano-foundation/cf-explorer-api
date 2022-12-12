package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface TokenProjection {

  Long getIdent();
  Timestamp getCreatedOn();
}
