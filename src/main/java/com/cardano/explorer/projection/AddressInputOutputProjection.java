package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface AddressInputOutputProjection {

  Long getTxId();

  String getTxHash();

  String getAddress();

  Integer getIndex();

  String getStakeAddress();

  BigDecimal getValue();

  BigDecimal getAssetQuantity();

  String getAssetName();

  String getAssetId();
}
