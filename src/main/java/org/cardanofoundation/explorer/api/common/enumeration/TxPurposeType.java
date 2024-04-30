package org.cardanofoundation.explorer.api.common.enumeration;

public enum TxPurposeType {
  // Four types of tx purpose
  SPEND,
  MINT,
  CERT,
  REWARD,
  VOTE,
  PROPOSE,
  ANY, // all types above
  NO_TX_PURPOSE // no tx purpose
}
