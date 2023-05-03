package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface StakeAddressMapper {

  @Mapping(target = "stakeKey", source = "stake.stakeAddress")
  @Mapping(target = "balance", source = "stake.totalStake")
  @Mapping(target = "poolId", source = "delegation.poolId")
  @Mapping(target = "tickerName", source = "delegation.tickerName")
  StakeFilterResponse fromStakeAddressAndDelegationProjection(StakeAddressProjection stake,
                                                              StakeDelegationProjection delegation);
}
