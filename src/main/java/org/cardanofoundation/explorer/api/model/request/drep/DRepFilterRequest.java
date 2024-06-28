package org.cardanofoundation.explorer.api.model.request.drep;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.springframework.format.annotation.DateTimeFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder
public class DRepFilterRequest {

  private String drepIdOrHash;
  private String anchorText;

  private BigInteger activeStakeFrom;
  private BigInteger activeStakeTo;

  private Double votingPowerFrom;
  private Double votingPowerTo;

  private DRepStatus drepStatus;

  private Double minGovParticipationRate;

  private Double maxGovParticipationRate;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime fromDate;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime toDate;
}
