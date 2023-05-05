package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.tx.WithdrawalResponse;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface WithdrawalMapper {

  @Mapping(target = "stakeAddressFrom", source = "addr.view")
  WithdrawalResponse fromWithdrawal(Withdrawal withdrawal);

}
