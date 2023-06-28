package org.cardanofoundation.explorer.api.model.response.dashboard;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonFormat;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;

import static org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig.DATE_TIME_FORMAT;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxGraph {
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  private Date date;
  private BigInteger simpleTransactions;
  private BigInteger smartContract;
  private BigInteger metadata;


  @Override
  public String toString(){
    return JsonUtil.getPrettyJson(this);
  }
}
