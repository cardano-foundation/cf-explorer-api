package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.tx.WithdrawalResponse;
import org.cardanofoundation.explorer.common.entity.ledgersync.Withdrawal;

@Mapper(componentModel = "spring")
public interface WithdrawalMapper {

  @Mapping(target = "stakeAddressFrom", source = "addr.view")
  WithdrawalResponse fromWithdrawal(Withdrawal withdrawal);
}
