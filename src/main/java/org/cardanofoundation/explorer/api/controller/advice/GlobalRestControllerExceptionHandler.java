package org.cardanofoundation.explorer.api.controller.advice;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import lombok.extern.log4j.Log4j2;
import org.apache.logging.log4j.util.Strings;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.common.exceptions.*;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import java.util.Arrays;
import java.util.Objects;

@Log4j2
@RestControllerAdvice
public class GlobalRestControllerExceptionHandler {

  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<ErrorResponse> handleValidationExceptions(MethodArgumentNotValidException ex) {
    FieldError fieldError = ex.getBindingResult().getFieldError();
    String errorMessage = "Invalid parameter";
    if (Objects.nonNull(fieldError)) {
      errorMessage = fieldError.getDefaultMessage();
    }
    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(CommonErrorCode.INVALID_PARAM.getCode())
                .errorMessage(errorMessage)
                .build());
  }

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
    log.warn("No rollback exception: {}, stack trace:", e.getMessage());
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
    log.warn("Refresh token exception: {}, stack trace:", e.getMessage());
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }

  @ExceptionHandler({AccessTokenExpireException.class})
  public ResponseEntity<ErrorResponse> handleAuthException(AccessTokenExpireException e) {
    log.warn("Access token expired: {}, stack trace:", e.getMessage());
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }

  @ExceptionHandler({InvalidAccessTokenException.class})
  public ResponseEntity<ErrorResponse> handleAuthException(InvalidAccessTokenException e) {
    log.warn("Invalid access token: {}", e.getErrorCode());
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }

  @ExceptionHandler({NoContentException.class})
  public ResponseEntity<BaseFilterResponse<?>> handleNoContent(NoContentException e) {
    return ResponseEntity.status(HttpStatus.OK)
            .body(new BaseFilterResponse<>());
  }

  @ExceptionHandler({ConstraintViolationException.class})
  public ResponseEntity<ErrorResponse> handleConstraintViolationException(ConstraintViolationException e) {
    log.warn("constraint not valid: {}", e.getMessage());

    String[] errors = e.getConstraintViolations().stream().map(ConstraintViolation::getMessage).filter(Strings::isNotBlank).toArray(String[]::new);

    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()))
                .errorMessage(Arrays.toString(errors))
                .build());
  }

  @ExceptionHandler({IllegalArgumentException.class})
  public ResponseEntity<ErrorResponse> handleIllegalArgumentException(IllegalArgumentException e) {
    log.warn("argument not valid: {}", e.getMessage());

    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()))
                .errorMessage(e.getMessage())
                .build());
  }

  @ExceptionHandler({MethodArgumentTypeMismatchException.class})
  public ResponseEntity<ErrorResponse> handleMethodArgumentTypeMismatch(MethodArgumentTypeMismatchException e) {
    log.warn("Argument type not valid: {}", e.getMessage());

    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
        .body(
            ErrorResponse.builder()
                .errorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()))
                .errorMessage(e.getName() + " not valid")
                .build());
  }

  @ExceptionHandler({UnauthorizedException.class})
  public ResponseEntity<ErrorResponse> handleMethodArgumentTypeMismatch(UnauthorizedException e) {
    log.warn("Argument type not valid: {}", e.getMessage());

    return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
        .body(
            ErrorResponse.builder()
                .errorCode(e.getErrorCode().getServiceErrorCode())
                .errorMessage(e.getErrorCode().getDesc())
                .build());
  }
}
