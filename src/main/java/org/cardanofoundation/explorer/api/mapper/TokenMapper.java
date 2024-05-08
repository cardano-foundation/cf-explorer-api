package org.cardanofoundation.explorer.api.mapper;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Value;

import com.bloxbean.cardano.client.address.util.AddressUtil;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.common.enumeration.AddressType;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;

@Mapper(
    componentModel = "spring",
    imports = {HexUtils.class},
    uses = {AssetMetadataMapper.class})
public abstract class TokenMapper {

  @Value("${application.token-logo-endpoint}")
  protected String tokenLogoEndpoint;

  @Mapping(
      target = "displayName",
      expression = "java(getDisplayName(multiAsset.getNameView(), multiAsset.getFingerprint()))")
  @Mapping(target = "createdOn", source = "time")
  public abstract TokenFilterResponse fromMultiAssetToFilterResponse(MultiAsset multiAsset);

  @Mapping(
      target = "displayName",
      expression = "java(HexUtils.fromHex(multiAsset.getName(), multiAsset.getFingerprint()))")
  @Mapping(target = "createdOn", source = "time")
  public abstract TokenResponse fromMultiAssetToResponse(MultiAsset multiAsset);

  @Mapping(
      target = "displayName",
      expression = "java(HexUtils.fromHex(projection.getTokenName(), projection.getFingerprint()))")
  @Mapping(target = "name", source = "tokenName")
  @Mapping(target = "metadata", expression = "java(getMetadata(projection))")
  @Mapping(target = "addressType", expression = "java(getAddressType(projection.getAddress()))")
  public abstract TokenAddressResponse fromAddressTokenProjection(
      AddressTokenProjection projection);

  //  @Mapping(
  //      target = "displayName",
  //      expression = "java(HexUtils.fromHex(multiAsset.getName(), multiAsset.getFingerprint()))")
  //  @Mapping(target = "policy", source = "multiAsset.policy")
  //  @Mapping(target = "fingerprint", source = "multiAsset.fingerprint")
  //  @Mapping(target = "quantity", source = "addressToken.balance")
  //  @Mapping(target = "address", ignore = true)
  //  @Mapping(target = "addressId", ignore = true)
  //  public abstract TokenAddressResponse fromMultiAssetAndAddressToken(
  //      MultiAsset multiAsset, AddressToken addressToken);

  LocalDateTime fromTimestamp(Timestamp timestamp) {
    return timestamp == null ? null : timestamp.toLocalDateTime();
  }

  String getDisplayName(String nameView, String fingerprint) {
    if (!StringUtils.isEmpty(nameView)) {
      return nameView;
    } else return fingerprint;
  }

  @Named("getTokenLogoURL")
  String getTokenLogoEndpoint(String logo) {
    return Objects.isNull(logo) ? null : (tokenLogoEndpoint + logo);
  }

  AddressType getAddressType(String address) {
    return address.startsWith("stake") ? AddressType.STAKE_ADDRESS : AddressType.PAYMENT_ADDRESS;
  }

  TokenMetadataResponse getMetadata(AddressTokenProjection projection) {
    if (StringUtils.isEmpty(projection.getSubject())) {
      return null;
    }
    TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse();
    tokenMetadataResponse.setUrl(projection.getUrl());
    tokenMetadataResponse.setTicker(projection.getTicker());
    tokenMetadataResponse.setLogo(getTokenLogoEndpoint(projection.getLogo()));
    tokenMetadataResponse.setDecimals(projection.getDecimals());
    tokenMetadataResponse.setDescription(projection.getDescription());
    return tokenMetadataResponse;
  }
}
