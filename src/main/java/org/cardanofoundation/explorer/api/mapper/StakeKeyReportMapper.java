package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.request.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;
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
  StakeKeyReportHistory toStakeKeyReportHistory(StakeKeyReportRequest stakeKeyReportRequest);

}
