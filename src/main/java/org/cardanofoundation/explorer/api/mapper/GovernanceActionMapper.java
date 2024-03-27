package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;

@Mapper(componentModel = "spring")
public interface GovernanceActionMapper {

  @Mapping(source = "status", target = "status")
  GovernanceActionResponse fromGovernanceActionProjection(
      GovernanceActionProjection governanceActionProjection);

  @Mapping(source = "type", target = "govActionType")
  @Mapping(
      target = "submissionDate",
      expression = "java(fromLong(govActionDetailsProjection.getBlockTime()))")
  GovernanceActionDetailsResponse fromGovActionDetailsProjection(
      GovActionDetailsProjection govActionDetailsProjection);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
