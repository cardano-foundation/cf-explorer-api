package org.cardanofoundation.explorer.api.model.request.drep;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class DRepFilterRequest {

  private String drepIdOrHash;
  private String anchorText;

  private BigInteger activeStakeFrom;
  private BigInteger activeStakeTo;

  private Double votingPowerFrom;
  private Double votingPowerTo;

  private DRepStatus drepStatus;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date fromDate;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date toDate;
}
