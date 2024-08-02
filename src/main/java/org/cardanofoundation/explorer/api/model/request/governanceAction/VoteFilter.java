package org.cardanofoundation.explorer.api.model.request.governanceAction;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import org.springframework.format.annotation.DateTimeFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VoteFilter {
  private String txHash;
  private Integer index;
  private VoterType voterType;
  private String voterHash;

  private BigInteger activeStakeFrom;
  private BigInteger activeStakeTo;

  private Double votingPowerFrom;
  private Double votingPowerTo;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime fromDate;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime toDate;
}
