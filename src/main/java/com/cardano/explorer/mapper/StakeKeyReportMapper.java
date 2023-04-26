package com.cardano.explorer.mapper;

import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface StakeKeyReportMapper {

  StakeKeyReportHistoryResponse toStakeKeyReportHistoryResponse(StakeKeyReportHistory stakeKeyReportHistory);

  @Mapping(target = "username", source = "stakeKeyReport.stakeKey")
  StakeKeyReportHistory toStakeKeyReportHistory(StakeKeyReport stakeKeyReport);

}
