package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;

@Mapper(componentModel = "spring")
public interface DRepCertificateMapper {

  @Mapping(source = "blockTime", target = "createdAt")
  DRepCertificateHistoryResponse fromDRepCertProjection(
      DRepCertificateProjection dRepCertificateProjection);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
