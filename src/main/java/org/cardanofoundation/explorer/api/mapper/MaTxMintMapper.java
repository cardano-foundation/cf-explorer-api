package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxMintingResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring",imports={HexUtils.class})
public interface MaTxMintMapper {

  @Mapping(target = "assetName",
      expression = "java(HexUtils.fromHex(maTxMint.getIdent().getName(), maTxMint.getIdent().getFingerprint()))")
  @Mapping(target = "policy", source = "ident.policy")
  @Mapping(target = "assetQuantity", source = "quantity")
  @Mapping(target = "assetId", expression = "java(maTxMint.getIdent().getFingerprint())")
  TxMintingResponse fromMaTxMint(MaTxMint maTxMint);

  @Mapping(target = "assetName",
      expression = "java(HexUtils.fromHex(input.getAssetName(), input.getAssetId()))")
  @Mapping(target = "assetId", expression = "java(input.getAssetId())")
  TxMintingResponse fromAddressInputOutputProjection(
      AddressInputOutputProjection input);

  @Mapping(target = "txHash", expression = "java(maTxMint.getTx().getHash())")
  @Mapping(target = "amount", source = "quantity")
  @Mapping(target = "time", source = "tx.block.time")
  TokenMintTxResponse fromMaTxMintToTokenMintTx(MaTxMint maTxMint);
}
