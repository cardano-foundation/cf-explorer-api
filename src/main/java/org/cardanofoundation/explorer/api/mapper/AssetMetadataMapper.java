package org.cardanofoundation.explorer.api.mapper;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Value;

import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public abstract class AssetMetadataMapper {

  @Value("${application.token-logo-endpoint}")
  protected String tokenLogoEndpoint;

  @Mapping(target = "logo", source = "logo", qualifiedByName = "getTokenLogoURL")
  public abstract TokenMetadataResponse fromAssetMetadata(AssetMetadata metadata);

  @Named("getTokenLogoURL")
  String getTokenLogoEndpoint(String logo){
    return Objects.isNull(logo) ? null : (tokenLogoEndpoint + logo);
  }
}
