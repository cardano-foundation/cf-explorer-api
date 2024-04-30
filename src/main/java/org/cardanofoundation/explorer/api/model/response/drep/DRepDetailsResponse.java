package org.cardanofoundation.explorer.api.model.response.drep;

import static org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig.DATE_TIME_FORMAT;

import java.math.BigInteger;
import java.util.Date;

import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepType;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DRepDetailsResponse {
  private String drepHash;
  private String drepId;
  private String anchorUrl;
  private String anchorHash;
  private Integer delegators;
  private BigInteger activeVoteStake;
  private BigInteger liveStake;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  private Date createdAt;

  @Enumerated(EnumType.STRING)
  private DRepStatus status;

  @Enumerated(EnumType.STRING)
  private DRepType type;

  private Float votingParticipation;
}
