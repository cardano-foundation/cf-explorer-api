package org.cardanofoundation.explorer.api.model.response.dashboard;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonFormat;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxGraph {
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
  private Date date;
  private BigInteger simpleTransactions;
  private BigInteger smartContract;
  private BigInteger metadata;


  @Override
  public String toString(){
    return JsonUtil.getPrettyJson(this);
  }
}
