package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.service.SupplyService;

@RestController
@RequestMapping("/api/v1/supply")
@RequiredArgsConstructor
@Tag(name = "supply", description = "ada token supply information")
public class SupplyController {

  private final SupplyService supplyService;

  @GetMapping("/circulating")
  @LogMessage
  @Operation(
      summary = "Get Circulating Supply",
      description = "returns the ADA token circulating supply",
      tags = {"supply"})
  public ResponseEntity<Long> getSupplyCirculating() {
    return ResponseEntity.ok(supplyService.getSupplyCirculating());
  }

  @GetMapping("/total")
  @LogMessage
  @Operation(
      summary = "Get Total Supply",
      description = "returns the ADA token total supply",
      tags = {"supply"})
  public ResponseEntity<Long> getSupplyTotal() {
    return ResponseEntity.ok(supplyService.getSupplyTotal());
  }
}
