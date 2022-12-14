package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.tx.TxMintingResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.cardano.explorer.util.HexUtils;
import com.sotatek.cardano.common.entity.MaTxMint;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring",imports={HexUtils.class})
public interface MaTxMintMapper {

  @Mapping(target = "assetName", expression = "java(HexUtils.fromHex(maTxMint.getIdent().getName()))")
  @Mapping(target = "policy.policyId", source = "ident.policy")
  @Mapping(target = "assetQuantity", source = "quantity")
  @Mapping(target = "assetId", expression = "java(maTxMint.getIdent().getFingerprint())")
  TxMintingResponse fromMaTxMint(MaTxMint maTxMint);

  @Mapping(target = "assetName",
      expression = "java(HexUtils.fromHex(input.getAssetName()))")
  @Mapping(target = "assetId", expression = "java(input.getAssetId())")
  TxMintingResponse fromAddressInputOutputProjection(
      AddressInputOutputProjection input);

  @Mapping(target = "txHash", expression = "java(maTxMint.getTx().getHash())")
  @Mapping(target = "amount", source = "quantity")
  @Mapping(target = "time", source = "tx.block.time")
  TokenMintTxResponse fromMaTxMintToTokenMintTx(MaTxMint maTxMint);
}
