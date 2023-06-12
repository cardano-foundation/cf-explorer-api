package org.cardanofoundation.explorer.api.model.request.stake.report;

import java.sql.Timestamp;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.cardanofoundation.explorer.api.controller.validate.StakeKeyLengthValid;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakeKeyReportRequest {

  @StakeKeyLengthValid
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
