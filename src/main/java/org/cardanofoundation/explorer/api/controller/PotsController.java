package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.PotsOverviewResponse;
import org.cardanofoundation.explorer.api.service.PotsService;

@RestController
@RequestMapping("/api/v1/pots")
@RequiredArgsConstructor
@Tag(name = "pots", description = "Ada token pots information")
public class PotsController {

  private final PotsService potsService;

  @GetMapping("/overview")
  @LogMessage
  @Operation(
      summary = "Get Pots Overview",
      description = "returns the ADA token pots overview",
      tags = {"pots"})
  public ResponseEntity<PotsOverviewResponse> getPotsOverview() {
    return ResponseEntity.ok(potsService.getPotsOverview());
  }
}
