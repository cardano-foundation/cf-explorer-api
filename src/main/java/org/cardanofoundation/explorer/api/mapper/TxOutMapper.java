package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.tx.TxOutResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;

@Mapper(componentModel = "spring")
public interface TxOutMapper {

  @Mapping(target = "stakeAddress", source = "stakeView")
  TxOutResponse fromAddressInputOutput(AddressInputOutputProjection addressInputOutputProjection);

  @Mapping(target = "address", source = "stakeAddress")
  TxOutResponse fromStakeAddressInputOutput(
      AddressInputOutputProjection addressInputOutputProjection);
}
