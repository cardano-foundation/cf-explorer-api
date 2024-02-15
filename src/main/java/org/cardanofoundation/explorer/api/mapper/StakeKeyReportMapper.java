package org.cardanofoundation.explorer.api.mapper;

import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.common.entity.explorer.StakeKeyReportHistory;

@Mapper(
    componentModel = "spring",
    imports = {Timestamp.class, LocalDateTime.class, Instant.class, ZoneOffset.class})
public interface StakeKeyReportMapper {

  @Mapping(target = "username", source = "stakeKeyReportHistory.reportHistory.username")
  @Mapping(target = "reportName", source = "stakeKeyReportHistory.reportHistory.reportName")
  @Mapping(target = "status", source = "stakeKeyReportHistory.reportHistory.status")
  @Mapping(target = "type", source = "stakeKeyReportHistory.reportHistory.type")
  @Mapping(target = "createdAt", source = "stakeKeyReportHistory.reportHistory.createdAt")
  StakeKeyReportHistoryResponse toStakeKeyReportHistoryResponse(
      StakeKeyReportHistory stakeKeyReportHistory);

  @Mapping(
      target = "reportHistory.createdAt",
      expression =
          "java(Timestamp.valueOf(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)))")
  @Mapping(target = "reportHistory.reportName", source = "reportName")
  StakeKeyReportHistory toStakeKeyReportHistory(StakeKeyReportRequest stakeKeyReportRequest);
}
