package com.cardano.explorer.mappers;

import com.cardano.explorer.entity.Epoch;
import com.cardano.explorer.model.EpochResponse;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface EpochMapper {

  EpochResponse epochToEpochResponse(Epoch epoch);

}
