package com.cardano.explorer.mapper;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.model.BlockFilterResponse;
import com.cardano.explorer.model.BlockResponse;
import com.sotatek.cardano.ledgersync.util.HexUtil;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface BlockMapper {
  BlockResponse blockToBlockResponse(Block block);

  BlockFilterResponse blockToBlockFilterResponse(Block block);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }

  default String fromByteaHash(byte[] hash) {
    return hash == null ? null : HexUtil.encodeHexString(hash);
  }

}
