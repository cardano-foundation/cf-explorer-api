package com.cardano.explorer.util.csv;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@Getter
@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
/**
 * This Enum class is used to map the column name in the CSV file to the title of the column
 * Each enum value should use for one field name of the one specific data object
 */
public enum ColumnTitleEnum {
  TX_HASH_TITLE("Transaction Hash"),
  EPOCH_TITLE("Epoch"),
  TIMESTAMP_TITLE("Timestamp"),
  REWARDS_PAID_TITLE("Rewards Paid"),
  AMOUNT_NET_TITLE("Net Amount"),
  AMOUNT_ADA_TITLE("Amount ADA"),
  DEPOSIT_TITLE("Hold"),
  FEES_TITLE("Fees"),
  TX_TYPE_TITLE("Transaction Type"),
  TX_STATUS_TITLE("Transaction Status"),
  STATUS_TITLE("Status");
  private String value;
}
