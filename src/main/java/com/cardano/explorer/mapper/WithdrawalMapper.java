package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.WithdrawalResponse;
import com.sotatek.cardano.common.entity.Withdrawal;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface WithdrawalMapper {

  @Mapping(target = "stakeAddressFrom", source = "addr.view")
  WithdrawalResponse fromWithdrawal(Withdrawal withdrawal);

}
