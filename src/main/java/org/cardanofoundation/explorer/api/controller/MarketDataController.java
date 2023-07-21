package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/markets")
@RequiredArgsConstructor
public class MarketDataController {

  private final MarketDataService marketDataService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Get ada cardano price, market cap, volume, and market related data")
  public ResponseEntity<Object> getMarketData(@RequestParam("currency") String currency) {
    return ResponseEntity.ok(marketDataService.getMarketData(currency));
  }
}