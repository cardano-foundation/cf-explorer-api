package org.cardanofoundation.explorer.api.projection;

public interface AddressTxCountProjection {
  String getAddress();

  Long getTxCount();

  void setAddress(String address);

  void setTxCount(Long txCount);
}
