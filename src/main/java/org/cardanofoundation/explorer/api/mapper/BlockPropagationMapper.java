package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.BlockPropagationResponse;
import org.cardanofoundation.explorer.api.projection.BlockPropagationProjection;

@Mapper(componentModel = "spring")
public interface BlockPropagationMapper {
  BlockPropagationResponse fromBlockPropagationProjection(
      BlockPropagationProjection blockPropagationProjection);
}
