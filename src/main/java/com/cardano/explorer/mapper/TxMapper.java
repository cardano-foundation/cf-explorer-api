package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.sotatek.cardano.common.entity.Tx;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface TxMapper {

  @Mapping(target = "epochNo", source = "block.epochNo")
  @Mapping(target = "slot", source = "block.slotNo")
  @Mapping(target = "blockNo", source = "block.blockNo")
  @Mapping(target = "totalOutput", source = "outSum")
  TxFilterResponse txToTxFilterResponse(Tx tx);

  @Mapping(target = "blockNo", source = "block.blockNo")
  @Mapping(target = "epochNo", source = "block.epochNo")
  @Mapping(target = "time", source = "block.time")
  @Mapping(target = "totalOutput", source = "outSum")
  TxResponse txToTxResponse(Tx tx);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }
}
