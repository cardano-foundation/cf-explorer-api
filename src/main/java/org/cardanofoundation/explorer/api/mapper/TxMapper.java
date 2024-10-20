package org.cardanofoundation.explorer.api.mapper;

import java.sql.Timestamp;
import java.time.LocalDateTime;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;

@Mapper(componentModel = "spring")
public interface TxMapper {

  @Mapping(target = "epochNo", source = "block.epochNo")
  @Mapping(target = "epochSlotNo", source = "block.epochSlotNo")
  @Mapping(target = "slot", source = "block.slotNo")
  @Mapping(target = "blockNo", source = "block.blockNo")
  @Mapping(target = "blockHash", source = "block.hash")
  @Mapping(target = "time", source = "block.time")
  TxFilterResponse txToTxFilterResponse(Tx tx);

  @Mapping(target = "tx.hash", source = "hash")
  @Mapping(target = "tx.blockNo", source = "block.blockNo")
  @Mapping(target = "tx.blockHash", source = "block.hash")
  @Mapping(target = "tx.epochSlot", source = "block.epochSlotNo")
  @Mapping(target = "tx.epochNo", source = "block.epochNo")
  @Mapping(target = "tx.time", source = "block.time")
  @Mapping(target = "tx.fee", source = "fee")
  @Mapping(target = "tx.slotNo", source = "block.slotNo")
  TxResponse txToTxResponse(Tx tx);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }
}
