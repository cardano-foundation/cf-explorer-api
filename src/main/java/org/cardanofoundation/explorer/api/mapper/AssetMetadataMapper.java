package org.cardanofoundation.explorer.api.mapper;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Value;

import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public abstract class AssetMetadataMapper {

  @Value("${application.token-logo-endpoint}")
  protected String tokenLogoEndpoint;

  @Mapping(target = "displayName", expression = "java(getDisplayName(multiAsset.getNameView(), multiAsset.getFingerprint()))")
  @Mapping(target = "createdOn", source = "time")
  public abstract TokenFilterResponse fromMultiAssetToFilterResponse(MultiAsset multiAsset);

  @Mapping(target = "createdOn", source = "time")
  @Mapping(target = "displayName", expression = "java(getDisplayName(tokenProjection.getNameView(), tokenProjection.getFingerprint()))")
  @Mapping(target = "metadata", expression = "java(getMetadata(tokenProjection))")
  public abstract TokenFilterResponse fromTokenProjectionToTokenFilterResponse(
      TokenProjection tokenProjection);

  @Mapping(target = "displayName", source = "nameView")
  @Mapping(target = "metadata", expression = "java(getMetadata(tokenProjection))")
  public abstract TokenFilterResponse fromTokenProjectionToFilterResponse(
      TokenProjection tokenProjection);

  @Mapping(target = "logo", source = "logo", qualifiedByName = "getTokenLogoURL")
  public abstract TokenMetadataResponse fromAssetMetadata(AssetMetadata metadata);

  @Named("getTokenLogoURL")
  String getTokenLogoEndpoint(String logo){
    return Objects.isNull(logo) ? null : (tokenLogoEndpoint + logo);
  }

  TokenMetadataResponse getMetadata(TokenProjection tokenProjection) {
    TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse();
    tokenMetadataResponse.setUrl(tokenProjection.getUrl());
    tokenMetadataResponse.setTicker(tokenProjection.getTicker());
    tokenMetadataResponse.setLogo(getTokenLogoEndpoint(tokenProjection.getLogo()));
    tokenMetadataResponse.setDecimals(tokenProjection.getDecimals());
    tokenMetadataResponse.setDescription(tokenProjection.getDescription());
    return tokenMetadataResponse;
  }

  String getDisplayName(String nameView, String fingerprint) {
    if (!StringUtils.isEmpty(nameView)) {
      return nameView;
    } else {
      return fingerprint;
    }
  }
}
