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
  POLICY_NOT_FOUND("404-POLICY_NOT_FOUND", "Policy not found"),
  STAKE_REGISTRATION_NOT_FOUND("404-STAKE_REGISTRATION_NOT_FOUND", "Stake registration not found"),
  STAKE_DE_REGISTRATION_NOT_FOUND("404-STAKE_DEREGISTRATION_NOT_FOUND", "Stake de-registration not found"),
  STAKE_DELEGATION_NOT_FOUND("404-STAKE_DELEGATION_NOT_FOUND", "Stake delegation not found"),
  STAKE_WITHDRAWAL_NOT_FOUND("404-STAKE_DELEGATION_NOT_FOUND", "Stake delegation not found"),
  STAKE_REPORT_HISTORY_NOT_FOUND("404-STAKE_REPORT_HISTORY_NOT_FOUND", "Stake report history not found"),
  POOL_REPORT_HISTORY_NOT_FOUND("404-POOL_REPORT_HISTORY_NOT_FOUND", "Pool report history not found"),
  REPORT_IS_IN_PROGRESS("404-REPORT_IS_IN_PROGRESS", "Report is in progress"),
  POOL_NOT_FOUND("404-POOL_NOT_FOUND", "PoolId not found"),
  EXPORT_TYPE_NOT_SUPPORTED("404-EXPORT_TYPE_NOT_SUPPORTED", "Export type not supported"),
  PROTOCOL_FIELD_NOT_FOUND("404-PROTOCOL_FIELD_NOT_FOUND","this field is not support"),
  SCRIPT_NOT_FOUND("404-SCRIPT_NOT_FOUND", "Script not found"),
  VERIFY_SCRIPT_FAILED("400-VERIFY_SCRIPT_FAILED", "Verify script failed"),
  FETCH_REWARD_ERROR("500-FETCH_REWARD_ERROR", "Fetch reward error"),
  TIME_RANGE_ILLEGAL("400-TIME_RANGE_ILLEGAL", "Time range is illegal"),
  REPORT_LIMIT_REACHED("400-REPORT_LIMIT_REACHED", "Report limit reached"),
  OUT_OF_QUERY_LIMIT("400-OUT_OF_QUERY_LIMIT", "Out of query limit");

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