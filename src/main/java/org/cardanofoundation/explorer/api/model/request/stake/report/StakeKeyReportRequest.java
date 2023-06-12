package org.cardanofoundation.explorer.api.model.request.stake.report;

import java.sql.Timestamp;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.controller.test.PrefixedValid;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakeKeyReportRequest {
  @PrefixedValid(CommonConstant.PREFIXED_STAKE_KEY)
  private String stakeKey;
  private String reportName;
  private Timestamp fromDate;
  private Timestamp toDate;
  private Boolean isADATransfer;
  private Boolean isFeesPaid;
  private Boolean eventRegistration;
  private Boolean eventDelegation;
  private Boolean eventRewards;
  private Boolean eventWithdrawal;
  private Boolean eventDeregistration;
}
