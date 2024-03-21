package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;

@Mapper(componentModel = "spring")
public interface DRepMapper {
  @Mapping(target = "createdAt", expression = "java(fromLong(dRepInfo.getCreatedAt()))")
  DRepDetailsResponse fromDrepInfo(DRepInfo dRepInfo);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
