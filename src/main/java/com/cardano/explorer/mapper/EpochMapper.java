package com.cardano.explorer.mapper;
import com.cardano.explorer.model.response.EpochResponse;
import com.sotatek.cardano.common.entity.Epoch;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface EpochMapper {

  EpochResponse epochToEpochResponse(Epoch epoch);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }
}
