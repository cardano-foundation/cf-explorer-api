package org.cardanofoundation.explorer.api.model.response.stake.report;

import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import java.sql.Timestamp;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeKeyReportHistoryResponse {
  private Long id;
  private String stakeKey;
  private String username;
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
  private ReportStatus status;
  private ReportType type;
}
