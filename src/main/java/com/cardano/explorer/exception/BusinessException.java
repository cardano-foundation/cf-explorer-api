package com.cardano.explorer.exception;

import lombok.Getter;

public final class BusinessException extends RuntimeException {

  @Getter
  private final transient ErrorResponse errorResponse;

  public BusinessException(ErrorResponse errorResponse) {
    this.errorResponse = errorResponse;
  }
}
