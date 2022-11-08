package com.cardano.explorer.exception;

import org.springframework.http.HttpStatus;

public class BusinessCode {

  private BusinessCode() {
  }
  public static final ErrorResponse INTERNAL_SERVER =
      new ErrorResponse("INTERNAL-SERVER", "Something went wrong", HttpStatus.INTERNAL_SERVER_ERROR);

  public static final ErrorResponse NOT_FOUND =
      new ErrorResponse("404", "Cam not find object for request", HttpStatus.NOT_FOUND);

}