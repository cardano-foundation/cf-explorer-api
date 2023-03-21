package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.stake.StakeFilterResponse;
import com.cardano.explorer.projection.StakeAddressProjection;
import com.cardano.explorer.projection.StakeDelegationProjection;
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
