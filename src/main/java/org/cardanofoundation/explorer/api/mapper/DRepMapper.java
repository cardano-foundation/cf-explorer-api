package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepRangeValuesResponse;
import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.DRepRangeProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.DRepInfo;

@Mapper(componentModel = "spring")
public interface DRepMapper {
  @Mapping(target = "createdAt", expression = "java(fromLong(dRepInfo.getCreatedAt()))")
  @Mapping(target = "votingParticipation", source = "govParticipationRate")
  DRepDetailsResponse fromDrepInfo(DRepInfo dRepInfo);

  @Mapping(
      target = "createdAt",
      expression = "java(fromLong(dRepDelegatorProjection.getBlockTime()))")
  DRepDelegatorsResponse fromDRepDelegatorProjection(
      DRepDelegatorProjection dRepDelegatorProjection);

  @Mapping(target = "createdAt", expression = "java(fromLong(dRepInfo.getCreatedAt()))")
  @Mapping(target = "updatedAt", expression = "java(fromLong(dRepInfo.getUpdatedAt()))")
  DRepFilterResponse fromDRepInfo(DRepInfo dRepInfo);

  DRepRangeValuesResponse fromDRepRangeProjection(DRepRangeProjection dRepRangeProjection);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
