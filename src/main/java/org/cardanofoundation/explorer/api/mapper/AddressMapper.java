package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface AddressMapper {

  ContractFilterResponse fromAddressToContractFilter(Address address);

  @Mapping(target = "stakeAddress", source = "stakeAddress.view")
  AddressResponse fromAddress(Address address);

  AddressFilterResponse fromAddressToFilterResponse(Address address);

}
