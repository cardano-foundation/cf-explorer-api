package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.TxDelegationResponse;
import com.sotatek.cardano.common.entity.Delegation;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface DelegationMapper {

  @Mapping(target = "address", source = "address.view")
  @Mapping(target = "poolId", source = "poolHash.view")
  TxDelegationResponse fromDelegation(Delegation delegation);

}
