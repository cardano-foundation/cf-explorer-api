package org.cardanofoundation.explorer.api.mapper;

import java.sql.Timestamp;
import java.time.LocalDateTime;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;

@Mapper(componentModel = "spring")
public interface EpochMapper {

  EpochResponse epochToEpochResponse(Epoch epoch);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }
}
