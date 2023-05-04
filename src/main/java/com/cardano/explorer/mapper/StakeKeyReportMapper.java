package com.cardano.explorer.mapper;

import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import java.sql.Timestamp;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring", imports={Timestamp.class})
public interface StakeKeyReportMapper {


  @Mapping(target = "username", source = "stakeKeyReportHistory.reportHistory.username")
  @Mapping(target = "reportName", source = "stakeKeyReportHistory.reportHistory.reportName")
  @Mapping(target = "status", source = "stakeKeyReportHistory.reportHistory.status")
  @Mapping(target = "type", source = "stakeKeyReportHistory.reportHistory.type")
  StakeKeyReportHistoryResponse toStakeKeyReportHistoryResponse(StakeKeyReportHistory stakeKeyReportHistory);

  @Mapping(target = "reportHistory.createdAt", expression = "java(new Timestamp(System.currentTimeMillis()))")
  @Mapping(target = "reportHistory.username", source = "stakeKey")
  @Mapping(target = "reportHistory.reportName", source = "reportName")
  StakeKeyReportHistory toStakeKeyReportHistory(StakeKeyReport stakeKeyReport);

}
