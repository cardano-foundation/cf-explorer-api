package com.cardano.explorer.exception;


import lombok.extern.log4j.Log4j2;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@Log4j2
@ControllerAdvice
public final class HandleExceptionAdvice {

  @ExceptionHandler(BusinessException.class)
  public ResponseEntity<ErrorResponse> handleBusinessException(
      BusinessException businessException) {
    HttpStatus httpStatus = businessException.getErrorResponse().getStatus();
    log.error("Exception Detail: {} and rootCause: {}",
        businessException.getErrorResponse().getMessage(),
        businessException.getErrorResponse().getMessage()
    );
    return new ResponseEntity<>(businessException.getErrorResponse(), httpStatus);
  }

  @ExceptionHandler(Exception.class)
  public ResponseEntity<ErrorResponse> handleBusinessException(Exception exception) {
    BusinessException businessException = new BusinessException(BusinessCode.INTERNAL_SERVER);
    if (exception.getMessage() != null && !exception.getMessage().isEmpty()) {
      log.error("Exception Detail: {} and rootCause: {}",
          exception.getMessage(),
          exception.getLocalizedMessage());
    }
    HttpStatus httpStatus = businessException.getErrorResponse().getStatus();
    return new ResponseEntity<>(businessException.getErrorResponse(), httpStatus);
  }


  @ExceptionHandler(Throwable.class)
  public ResponseEntity<ErrorResponse> handleBusinessException(Throwable exception) {
    BusinessException businessException = new BusinessException(BusinessCode.INTERNAL_SERVER);
    HttpStatus httpStatus = businessException.getErrorResponse().getStatus();
    log.error("Exception global with message: {} status: {} exception: {}", exception.getMessage(),
        httpStatus, exception.getLocalizedMessage());
    return new ResponseEntity<>(businessException.getErrorResponse(), httpStatus);
  }
}
