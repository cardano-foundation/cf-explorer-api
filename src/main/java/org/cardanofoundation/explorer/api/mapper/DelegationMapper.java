package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.tx.TxDelegationResponse;
import org.cardanofoundation.explorer.common.entity.ledgersync.Delegation;

@Mapper(componentModel = "spring")
public interface DelegationMapper {

  @Mapping(target = "address", source = "address.view")
  @Mapping(target = "poolId", source = "poolHash.view")
  TxDelegationResponse fromDelegation(Delegation delegation);
}
