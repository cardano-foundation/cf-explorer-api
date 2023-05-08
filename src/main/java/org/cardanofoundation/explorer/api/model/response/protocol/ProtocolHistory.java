package org.cardanofoundation.explorer.api.model.response.protocol;

import java.util.Date;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProtocolHistory {

  Date time;
  String transactionHash;
  Object value;
  ProtocolHistory oldValue;
}


