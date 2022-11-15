package com.cardano.explorer.mapper;

import com.cardano.explorer.entity.projection.AddressInputOutput;
import com.cardano.explorer.model.response.TxOutResponse;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  TxOutResponse fromAddressInputOutput(AddressInputOutput addressInputOutput);
}
