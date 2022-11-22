package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.TxOutResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  TxOutResponse fromAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);
}
