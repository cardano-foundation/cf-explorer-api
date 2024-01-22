package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class AddressTokenProjectionImpl implements AddressTokenProjection {

  BigInteger quantity;
  String fingerprint;
  String tokenName;
  String address;
  Long addressId;
  String policy;
  Long multiAssetId;
  String url;
  String ticker;
  Integer decimals;
  String logo;
  String description;
  String subject;

  @Override
  public BigInteger getQuantity() {
    return this.quantity;
  }

  @Override
  public String getFingerprint() {
    return this.fingerprint;
  }

  @Override
  public String getTokenName() {
    return this.tokenName;
  }

  @Override
  public String getAddress() {
    return this.address;
  }

  @Override
  public Long getAddressId() {
    return this.addressId;
  }

  @Override
  public String getPolicy() {
    return this.policy;
  }

  @Override
  public Long getMultiAssetId() {
    return this.multiAssetId;
  }

  @Override
  public String getUrl() {
    return this.url;
  }

  @Override
  public String getTicker() {
    return this.ticker;
  }

  @Override
  public Integer getDecimals() {
    return this.decimals;
  }

  @Override
  public String getLogo() {
    return this.logo;
  }

  @Override
  public String getDescription() {
    return this.description;
  }

  @Override
  public String getSubject() {
    return this.subject;
  }
}
