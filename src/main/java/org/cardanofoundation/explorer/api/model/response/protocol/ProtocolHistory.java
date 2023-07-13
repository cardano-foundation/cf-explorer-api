package org.cardanofoundation.explorer.api.model.response.protocol;

import java.time.LocalDateTime;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolStatus;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(value = Include.NON_NULL)
public class ProtocolHistory {

  LocalDateTime time;
  String transactionHash;
  Object value;
  ProtocolStatus status;
  @JsonIgnore
  Long costModelId;
  Integer epochNo;


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof ProtocolHistory)) {
      return false;
    }
    ProtocolHistory that = (ProtocolHistory) o;

    if (Objects.isNull(value) || Objects.isNull(that.value)) {
      return Objects.isNull(value) && Objects.isNull(that.value);
    }

    return Objects.equals(value.hashCode(), that.value.hashCode());
  }


  @Override
  public int hashCode() {
    return CommonConstant.hashCode(value, status);
  }
}


