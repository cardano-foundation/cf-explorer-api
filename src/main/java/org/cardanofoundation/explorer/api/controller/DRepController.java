package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.service.DRepCertificateService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/dreps")
@RequiredArgsConstructor
@Validated
@Tag(name = "drep", description = "The delegated representatives APIs")
public class DRepController {

  private final DRepCertificateService dRepCertificateService;

  @GetMapping("/certificates-history/{drepHashOrDrepId}")
  @LogMessage
  @Operation(
      summary = "Get list of DRep certificates history",
      tags = {"drep"})
  public ResponseEntity<BaseFilterResponse<DRepCertificateHistoryResponse>>
      getTxDRepCertificatesHistory(
          @PathVariable @Parameter(description = "The DRep id or DRep hash")
              String drepHashOrDrepId,
          @ParameterObject
              @PaginationValid
              @PaginationDefault(
                  size = 20,
                  sort = {"createdAt"},
                  direction = Sort.Direction.DESC)
              @Valid
              Pagination pagination) {
    return ResponseEntity.ok(
        dRepCertificateService.getTxDRepCertificateHistory(
            drepHashOrDrepId, pagination.toPageable()));
  }
}
