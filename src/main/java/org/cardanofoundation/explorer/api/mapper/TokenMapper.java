package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring", imports = {HexUtils.class}, uses = {AssetMetadataMapper.class})
public interface TokenMapper {

  @Mapping(target = "displayName",
      expression = "java(HexUtils.fromHex(multiAsset.getName(), multiAsset.getFingerprint()))")
  @Mapping(target = "createdOn", source = "time")
  TokenFilterResponse fromMultiAssetToFilterResponse(MultiAsset multiAsset);

  @Mapping(target = "displayName",
      expression = "java(HexUtils.fromHex(multiAsset.getName(), multiAsset.getFingerprint()))")
  @Mapping(target = "createdOn", source = "time")
  TokenResponse fromMultiAssetToResponse(MultiAsset multiAsset);

  @Mapping(target = "displayName",
      expression = "java(HexUtils.fromHex(projection.getTokenName(), projection.getFingerprint()))")
  @Mapping(target = "name", source = "tokenName")
  TokenAddressResponse fromAddressTokenProjection(AddressTokenProjection projection);


  @Mapping(target = "displayName",
      expression = "java(HexUtils.fromHex(multiAsset.getName(), multiAsset.getFingerprint()))")
  @Mapping(target = "policy", source = "multiAsset.policy")
  @Mapping(target = "fingerprint", source = "multiAsset.fingerprint")
  TokenAddressResponse fromMultiAssetAndAddressToken(MultiAsset multiAsset, AddressTokenProjection projection);

  default LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }
}
