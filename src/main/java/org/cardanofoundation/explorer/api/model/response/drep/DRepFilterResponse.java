package org.cardanofoundation.explorer.api.model.response.drep;

import static org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig.DATE_TIME_FORMAT;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DRepFilterResponse {

  String dRepId;
  String dRepHash;
  String anchorUrl;
  String anchorHash;
  BigInteger activeStake;
  Double votingPower;
  DRepStatus status;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  Date registeredAt;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  Date lastUpdatedAt;
}
