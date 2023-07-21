package org.cardanofoundation.explorer.api.model.response;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class ReportLimitResponse {

  private Integer limitPer24hours;
  private Boolean isLimitReached;
}
