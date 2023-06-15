package org.cardanofoundation.explorer.api.exception;

import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.common.exceptions.enums.ErrorCode;

@Getter
@Setter
public class FetchRewardException extends RuntimeException{
  private final String errorCode;

  private final String errorMsg;

  public FetchRewardException(ErrorCode errorCode) {
    this.errorCode = errorCode.getServiceErrorCode();
    this.errorMsg = errorCode.getDesc();
  }
}
