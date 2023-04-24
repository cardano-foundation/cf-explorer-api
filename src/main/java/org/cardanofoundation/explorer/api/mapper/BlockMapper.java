package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import com.sotatek.cardano.common.entity.Block;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface BlockMapper {

  @Mapping(target = "slotLeader", source = "slotLeader.hash")
  BlockResponse blockToBlockResponse(Block block);

  @Mapping(target = "slotLeader", source = "slotLeader.hash")
  BlockFilterResponse blockToBlockFilterResponse(Block block);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }

}
