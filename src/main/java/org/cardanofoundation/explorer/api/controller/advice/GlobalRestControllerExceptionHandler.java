package org.cardanofoundation.explorer.api.controller.advice;

import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.common.exceptions.*;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@Log4j2
@RestControllerAdvice
public class GlobalRestControllerExceptionHandler {
  @ExceptionHandler({BusinessException.class})
  public ResponseEntity<ErrorResponse> handleException(BusinessException e) {
    log.warn("Business logic exception: {}, stack trace: {}", e.getMessage(), e.getErrorMsg());
    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode())
                .errorMessage(e.getErrorMsg())
                .build());
  }

  @ExceptionHandler({NoContentException.class})
  public ResponseEntity<ErrorResponse> handleNoContent(NoContentException e) {
    log.warn("No content");
    return ResponseEntity.status(HttpStatus.OK)
            .body(ErrorResponse.builder()
                    .errorCode(HttpStatus.NO_CONTENT.toString())
                    .errorMessage(e.getErrorMsg())
                    .build());
  }

  @ExceptionHandler({FetchRewardException.class})
  public ResponseEntity<ErrorResponse> handleException(FetchRewardException e) {
    log.warn("Business logic exception: {}, stack trace: {}", e.getMessage(), e.getErrorMsg());
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode())
                .errorMessage(e.getErrorMsg())
                .build());
  }

  @ExceptionHandler({IgnoreRollbackException.class})
  public ResponseEntity<ErrorResponse> handleException(IgnoreRollbackException e) {
    log.warn("No rollback exception: {}, stack trace:", e.getMessage(), e);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode())
                .errorMessage(e.getErrorMsg())
                .build());
  }

  @ExceptionHandler({Exception.class})
  public ResponseEntity<ErrorResponse> handleException(Exception e) {
    log.warn("Unknown exception: {}, stack trace:", e.getMessage(), e);
    return new ResponseEntity<>(
        ErrorResponse.builder()
            .errorCode(CommonErrorCode.UNKNOWN_ERROR.getServiceErrorCode())
            .errorMessage(CommonErrorCode.UNKNOWN_ERROR.getDesc())
            .build(),
        HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @ExceptionHandler({TokenRefreshException.class})
  public ResponseEntity<ErrorResponse> handleAuthException(TokenRefreshException e) {
    log.warn("Refresh token exception: {}, stack trace:", e.getMessage(), e);
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }

  @ExceptionHandler({AccessTokenExpireException.class})
  public ResponseEntity<ErrorResponse> handleAuthException(AccessTokenExpireException e) {
    log.warn("Access token expired: {}, stack trace:", e.getMessage(), e);
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }

  @ExceptionHandler({InvalidAccessTokenException.class})
  public ResponseEntity<ErrorResponse> handleAuthException(InvalidAccessTokenException e) {
    log.warn("Invalid access token: {}", e.getErrorCode(), e);
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }
}
