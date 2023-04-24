package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.tx.TxOutResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  TxOutResponse fromAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);

  @Mapping(target = "address", source = "stakeAddress")
  TxOutResponse fromStakeAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);
}
