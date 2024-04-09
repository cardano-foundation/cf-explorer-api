package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepFilterResponse;
import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;

@Mapper(componentModel = "spring")
public interface DRepMapper {
  @Mapping(target = "createdAt", expression = "java(fromLong(dRepInfo.getCreatedAt()))")
  DRepDetailsResponse fromDrepInfo(DRepInfo dRepInfo);

  @Mapping(
      target = "createdAt",
      expression = "java(fromLong(dRepDelegatorProjection.getBlockTime()))")
  DRepDelegatorsResponse fromDRepDelegatorProjection(
      DRepDelegatorProjection dRepDelegatorProjection);

  @Mapping(target = "registeredAt", expression = "java(fromLong(dRepInfo.getCreatedAt()))")
  @Mapping(target = "lastUpdatedAt", expression = "java(fromLong(dRepInfo.getUpdatedAt()))")
  @Mapping(target = "activeStake", source = "activeVoteStake")
  @Mapping(target = "dRepId", source = "drepId")
  @Mapping(target = "dRepHash", source = "drepHash")
  DRepFilterResponse fromDRepInfo(DRepInfo dRepInfo);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
