package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.model.response.contract.ContractFilterResponse;
import com.sotatek.cardano.common.entity.Address;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface AddressMapper {

  ContractFilterResponse fromAddressToContractFilter(Address address);

  @Mapping(target = "isContract", source = "addressHasScript")
  @Mapping(target = "stakeAddress", source = "stakeAddress.view")
  AddressResponse fromAddress(Address address);

}
