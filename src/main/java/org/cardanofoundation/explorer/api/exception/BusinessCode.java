package org.cardanofoundation.explorer.api.exception;

import org.cardanofoundation.explorer.common.exceptions.enums.ErrorCode;

public enum BusinessCode implements ErrorCode {
  INTERNAL_ERROR("INTERNAL-SERVER", "Something went wrong"),
  EPOCH_NOT_FOUND("404-EPOCH_NOT_FOUND", "Epoch not found"),
  BLOCK_NOT_FOUND("404-BLOCK_NOT_FOUND", "Block not found"),
  TRANSACTION_NOT_FOUND("404-TRANSACTION_NOT_FOUND", "Transaction not found"),
  ADDRESS_NOT_FOUND("404-ADDRESS_NOT_FOUND", "Wallet address not found"),
  STAKE_ADDRESS_NOT_FOUND("404-STAKE_ADDRESS_NOT_FOUND", "Stake address not found"),
  TOKEN_NOT_FOUND("404-TOKEN_NOT_FOUND", "Token not found"),
  POLICY_NOT_FOUND("404-POLICY_NOT_FOUND", "Policy not found");

  private final String code;
  private final String desc;

  BusinessCode(String code, String desc) {
    this.code = code;
    this.desc = desc;
  }

  public String getCode() {
    return this.code;
  }

  public String getDesc() {
    return this.desc;
  }

  public String getServicePrefix() {
    return "BC";
  }

}