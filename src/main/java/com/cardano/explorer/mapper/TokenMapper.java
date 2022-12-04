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
  @Mapping(target = "name",
      expression = "java(HexUtils.fromHex(multiAsset.getName()))")
  @Mapping(target = "policy", expression = "java(multiAsset.getFingerprint())")
  @Mapping(target = "fingerprint", expression = "java(multiAsset.getFingerprint())")
  TokenFilterResponse fromMultiAssetToFilterResponse(MultiAsset multiAsset);

  @Mapping(target = "name",
      expression = "java(HexUtils.fromHex(multiAsset.getName()))")
  @Mapping(target = "policy", expression = "java(multiAsset.getFingerprint())")
  @Mapping(target = "fingerprint", expression = "java(multiAsset.getFingerprint())")
  TokenResponse fromMultiAssetToResponse(MultiAsset multiAsset);

  @Mapping(target = "tokenName",
      expression = "java(HexUtils.fromHex(projection.getTokenName()))")
  @Mapping(target = "address", expression = "java(projection.getAddress())")
  @Mapping(target = "fingerprint", expression = "java(projection.getFingerprint())")
  TokenAddressResponse fromAddressTokenProjection(AddressTokenProjection projection);

}
