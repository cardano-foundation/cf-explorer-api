package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;

@Mapper(componentModel = "spring")
public interface GovernanceActionMapper {
  GovernanceActionResponse fromGovernanceActionProjection(
      GovernanceActionProjection governanceActionProjection);
}
