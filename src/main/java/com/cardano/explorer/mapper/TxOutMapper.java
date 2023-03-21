package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.TxOutResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  TxOutResponse fromAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);

  @Mapping(target = "address", source = "stakeAddress")
  TxOutResponse fromStakeAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);
}
