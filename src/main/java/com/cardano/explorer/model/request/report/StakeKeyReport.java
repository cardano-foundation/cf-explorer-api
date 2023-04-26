package com.cardano.explorer.model.request.report;

import com.sotatek.cardano.common.entity.StakingLifeCycleEvent;
import java.sql.Timestamp;
import java.util.Set;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeKeyReport {

  private String stakeKey;
  private String reportName;
  private Timestamp fromDate;
  private Timestamp toDate;
  private Boolean isADATransfer;
  private Boolean isFeesPaid;
  private Set<StakingLifeCycleEvent> stakingLifeCycleEvents;
}
