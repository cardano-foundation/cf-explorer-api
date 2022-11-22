package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import com.sotatek.cardano.common.entity.Block;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface BlockMapper {

  @Mapping(target = "slotLeader", source = "slotLeader.hash")
  BlockResponse blockToBlockResponse(Block block);

  BlockFilterResponse blockToBlockFilterResponse(Block block);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }

}
