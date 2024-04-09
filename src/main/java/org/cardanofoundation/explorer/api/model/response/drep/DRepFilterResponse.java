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

  String drepId;
  String drepHash;
  String anchorUrl;
  String anchorHash;
  BigInteger activeVoteStake;
  Double votingPower;
  DRepStatus status;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  Date createdAt;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  Date updatedAt;
}
