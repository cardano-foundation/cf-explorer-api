package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;

import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;

@Mapper(componentModel = "spring")
public interface AddressMapper {

  ContractFilterResponse fromAddressToContractFilter(Address address);

  //  @Mapping(target = "stakeAddress", source = "stakeAddress.view")
  //  AddressResponse fromAddress(Address address);

  AddressFilterResponse fromAddressToFilterResponse(Address address);
}
