package com.cardano.explorer.mapper;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import com.sotatek.cardano.ledgersync.util.HexUtil;
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

  default String fromByteaHash(byte[] hash) {
    return hash == null ? null : HexUtil.encodeHexString(hash);
  }

}
