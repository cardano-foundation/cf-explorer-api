package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pools")
@RequiredArgsConstructor
public class PoolController {

  private final PoolRegistrationService poolRegistrationService;

  @GetMapping("/registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(
      @ParameterObject @PageableDefault() Pageable pageable) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolRegistration(pageable));
  }

  @GetMapping("/de-registration")
  @LogMessage
  public ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(
      @ParameterObject @PageableDefault() Pageable pageable) {
    return ResponseEntity.ok(poolRegistrationService.getDataForPoolDeRegistration(pageable));
  }
}
