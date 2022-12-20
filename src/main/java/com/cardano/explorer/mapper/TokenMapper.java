package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.util.HexUtils;
import com.sotatek.cardano.common.entity.MultiAsset;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring", imports={HexUtils.class})
public interface TokenMapper {
  @Mapping(target = "displayName", expression = "java(HexUtils.fromHex(multiAsset.getName()))")
  @Mapping(target = "createdOn", source = "time")
  TokenFilterResponse fromMultiAssetToFilterResponse(MultiAsset multiAsset);

  @Mapping(target = "displayName", expression = "java(HexUtils.fromHex(multiAsset.getName()))")
  TokenResponse fromMultiAssetToResponse(MultiAsset multiAsset);

  @Mapping(target = "displayName", expression = "java(HexUtils.fromHex(projection.getTokenName()))")
  @Mapping(target = "name", source = "tokenName")
  TokenAddressResponse fromAddressTokenProjection(AddressTokenProjection projection);

}
