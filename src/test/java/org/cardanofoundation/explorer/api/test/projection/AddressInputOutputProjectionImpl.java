package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class AddressInputOutputProjectionImpl implements AddressInputOutputProjection {
  Long txId;
  String txHash;
  String address;
  Integer index;
  String stakeAddress;
  BigInteger value;
  BigInteger assetQuantity;
  String assetName;
  String assetId;
  String assetsJson;
  String stakeView;
  Long multiAssetId;

  @Override
  public Long getTxId() {
    return this.txId;
  }

  @Override
  public String getTxHash() {
    return this.txHash;
  }

  @Override
  public String getAddress() {
    return this.address;
  }

  @Override
  public Integer getIndex() {
    return this.index;
  }

  @Override
  public String getStakeAddress() {
    return this.stakeAddress;
  }

  @Override
  public BigInteger getValue() {
    return this.value;
  }

  @Override
  public BigInteger getAssetQuantity() {
    return this.assetQuantity;
  }

  @Override
  public String getAssetName() {
    return this.assetName;
  }

  @Override
  public String getAssetId() {
    return this.assetId;
  }

  @Override
  public String getAssetsJson() {
    return this.assetsJson;
  }

  @Override
  public String getStakeView() {
    return this.stakeView;
  }

  @Override
  public Long getMultiAssetId() {
    return this.multiAssetId;
  }
}
