package org.cardanofoundation.explorer.api.model.response.protocol;

import java.math.BigInteger;
import java.sql.Timestamp;
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

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.HASH_LENGTH;

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
  @JsonIgnore
  Timestamp starTimestamp;
  @JsonIgnore
  Timestamp endTimestamp;


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

  public int hashCode(Object... a) {
    if (a == null) {
      return -BigInteger.ONE.intValue();
    }

    int result = BigInteger.ONE.intValue();

    for (Object element : a) {
      result = HASH_LENGTH * result + (element == null ? -BigInteger.ONE.intValue()
                                                       : element.hashCode());
    }

    return result;
  }

  @Override
  public int hashCode() {
    return CommonConstant.hashCode(value, status);
  }
}


