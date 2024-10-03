package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BolnisiProjectNumberResponse;
import org.cardanofoundation.explorer.api.service.BolnisiMetadataService;

@RestController
@RequestMapping("/api/v1/bolnisi")
@RequiredArgsConstructor
@Validated
@Tag(name = "Bolnisi", description = "The bolnisi APIs")
public class BolnisiController {
  private final BolnisiMetadataService bolnisiMetadataService;

  @GetMapping("/overview")
  @LogMessage
  @Operation(
      summary = "Get an overview of the Bolnisi project number",
      tags = {"bolnisi"})
  public ResponseEntity<BolnisiProjectNumberResponse> getBolnisiProjectNumber() {
    return ResponseEntity.ok(bolnisiMetadataService.getBolnisiProjectNumber());
  }
}
