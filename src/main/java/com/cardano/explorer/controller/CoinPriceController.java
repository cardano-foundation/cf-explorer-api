package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.service.CoinPriceService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/coin")
@RequiredArgsConstructor
public class CoinPriceController {

  private final CoinPriceService coinPriceService;

  @GetMapping("/price")
  @LogMessage
  @Operation(summary = "Get current price of cardano coin in usd")
  public ResponseEntity<?> getCardanoCoinPrice() {
    return ResponseEntity.ok(coinPriceService.getCoinPrice());
  }
}
