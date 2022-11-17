package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.TxOutResponse;
import com.cardano.explorer.projection.AddressInputOutput;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  TxOutResponse fromAddressInputOutput(AddressInputOutput addressInputOutput);
}
