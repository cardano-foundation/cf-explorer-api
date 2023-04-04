package com.cardano.explorer.projection;

import java.math.BigInteger;

public interface AddressInputOutputProjection {

  Long getTxId();

  String getTxHash();

  String getAddress();

  Integer getIndex();

  String getStakeAddress();

  BigInteger getValue();

  BigInteger getAssetQuantity();

  String getAssetName();

  String getAssetId();

  String getAssetsJson();
}
