package org.cardanofoundation.explorer.api.model.request.stake.report;

import java.sql.Timestamp;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.requestbody.JsonPattern;
import org.cardanofoundation.explorer.common.validation.date.requestbody.TimestampValidDeserialize;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakeKeyReportRequest {

  private String stakeKey;
  private String reportName;
  @JsonDeserialize(using =  TimestampValidDeserialize.class)
  @JsonPattern(pattern = DatePattern.YYYY_MM_DD_HH_MM_SS)
  private Timestamp fromDate;
  @JsonDeserialize(using =  TimestampValidDeserialize.class)
  @JsonPattern(pattern = DatePattern.YYYY_MM_DD_HH_MM_SS)
  private Timestamp toDate;
  private Boolean isADATransfer;
  private Boolean isFeesPaid;
  private Boolean eventRegistration;
  private Boolean eventDelegation;
  private Boolean eventRewards;
  private Boolean eventWithdrawal;
  private Boolean eventDeregistration;
}
