package org.cardanofoundation.explorer.api.exception;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.common.exception.ErrorCode;

@Getter
@Setter
public class NoContentException extends RuntimeException {
  private final String errorCode;

  private final String errorMsg;

  public NoContentException(ErrorCode err) {
    this.errorCode = err.getServiceErrorCode();
    this.errorMsg = err.getDesc();
  }
}
