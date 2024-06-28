package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;

@Mapper(componentModel = "spring")
public interface AddressMapper {

  AddressResponse fromAddress(Address address);
}
