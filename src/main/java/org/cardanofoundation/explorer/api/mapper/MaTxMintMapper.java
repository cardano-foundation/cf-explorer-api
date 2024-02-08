package org.cardanofoundation.explorer.api.mapper;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Value;

import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxMintingResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.MintProjection;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
@Mapper(componentModel = "spring",imports={HexUtils.class})
public abstract class MaTxMintMapper {

  @Value("${application.token-logo-endpoint}")
  protected String tokenLogoEndpoint;

  @Mapping(target = "assetName",
      expression = "java(HexUtils.fromHex(input.getAssetName(), input.getAssetId()))")
  @Mapping(target = "assetId", expression = "java(input.getAssetId())")
  public abstract TxMintingResponse fromAddressInputOutputProjection(
      AddressInputOutputProjection input);

  @Mapping(target = "txHash", expression = "java(maTxMint.getTx().getHash())")
  @Mapping(target = "amount", source = "quantity")
  @Mapping(target = "time", source = "tx.block.time")
  public abstract TokenMintTxResponse fromMaTxMintToTokenMintTx(MaTxMint maTxMint);

  @Mapping(target = "assetName", expression = "java(HexUtils.fromHex(mintProjection.getName(), mintProjection.getFingerprint()))")
  @Mapping(target = "policy", source = "policy")
  @Mapping(target = "assetQuantity", source = "assetQuantity")
  @Mapping(target = "assetId", source = "fingerprint")
  @Mapping(target = "metadata", expression = "java(getMetadata(mintProjection))")
  public abstract TxMintingResponse fromMintProjectionToTxMintingResponse(
      MintProjection mintProjection);


  @Named("getTokenLogoURL")
  String getTokenLogoEndpoint(String logo) {
    return Objects.isNull(logo) ? null : (tokenLogoEndpoint + logo);
  }

  TokenMetadataResponse getMetadata(MintProjection mintProjection) {
    TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse();
    tokenMetadataResponse.setUrl(mintProjection.getUrl());
    tokenMetadataResponse.setTicker(mintProjection.getTicker());
    tokenMetadataResponse.setLogo(getTokenLogoEndpoint(mintProjection.getLogo()));
    tokenMetadataResponse.setDecimals(mintProjection.getDecimals());
    tokenMetadataResponse.setDescription(mintProjection.getDescription());
    return tokenMetadataResponse;
  }
}
