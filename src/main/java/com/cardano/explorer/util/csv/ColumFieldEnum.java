package com.cardano.explorer.util.csv;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;


@Getter
@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
/**
 * This Enum class is used to map the column name in the CSV file to the field name of the data object
 * Each enum value should have the same name as the field name of the data object
 * Each enum value should use for one field name of the one specific data object
 */
public enum ColumFieldEnum {

  TX_HASH_COLUMN("txHash"),
  EPOCH_COLUMN("epoch"),
  TIME_COLUMN("time"),
  AMOUNT_COLUMN("amount"),
  NET_AMOUNT_COLUMN("value"),
  DEPOSIT_COLUMN("deposit"),
  FEE_COLUMN("fee"),
  TYPE_COLUMN("type"),
  STATUS_COLUMN("status"),
  EPOCH_NO_COLUMN("epochNo"),
  OUT_SUM_COLUMN("outSum");

  private String value;
}
