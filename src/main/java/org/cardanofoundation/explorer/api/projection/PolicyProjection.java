package org.cardanofoundation.explorer.api.projection;

public interface PolicyProjection {
  String getPolicy();
  Integer getNumberOfTokens();
  Integer getNumberOfAssetHolders();
}
