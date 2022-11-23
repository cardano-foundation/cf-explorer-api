package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.TxMintingResponse;
import com.sotatek.cardano.common.entity.MaTxMint;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface MaTxMintMapper {
  @Mapping(target = "assetName", source = "ident.name")
  @Mapping(target = "amount", source = "quantity")
  @Mapping(target = "policy.policyId", source = "ident.hash")
  TxMintingResponse fromMaTxMint(MaTxMint maTxMint);
}
