package org.cardanofoundation.explorer.api.model.response.protocol;

import java.util.Date;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolStatus;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProtocolHistory {

  Date time;
  String transactionHash;
  Object value;
  ProtocolStatus status;
  @JsonIgnore
  Long costModelId;

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof ProtocolHistory)) {
      return false;
    }
    ProtocolHistory that = (ProtocolHistory) o;
    return Objects.equals(value.hashCode(), that.value.hashCode());
  }


  @Override
  public int hashCode() {
    return CommonConstant.hashCode(value, status);
  }
}


