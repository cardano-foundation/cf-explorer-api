package com.cardano.explorer.model.response.report;

import com.sotatek.cardano.common.entity.StakingLifeCycleEvent;
import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import java.sql.Timestamp;
import java.util.Set;
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
  private ReportStatus status;
  private ReportType type;
  private Set<StakingLifeCycleEvent> stakingLifeCycleEvents;
}
