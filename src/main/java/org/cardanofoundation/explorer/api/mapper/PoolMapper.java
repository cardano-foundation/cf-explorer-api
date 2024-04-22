package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;

@Mapper(componentModel = "spring")
public interface PoolMapper {
  PoolRangeValuesResponse fromPoolRangeProjection(PoolRangeProjection poolRangeProjection);
}
