package org.cardanofoundation.explorer.api.controller;

import java.math.BigDecimal;
import java.math.BigInteger;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@RestController
@RequestMapping("/api/v1/supply")
@RequiredArgsConstructor
@Tag(name = "supply", description = "ada token supply information")
public class SupplyController {

  //  ➜  ~ curl -H 'Host: explorer.cardano.org' --insecure https://147.75.85.167/supply/circulating
  // 35969368510.862%
  // ➜  ~ curl -H 'Host: explorer.cardano.org' --insecure https://147.75.85.167/supply/total
  // 37170426657.961%

  private final CardanoConverters cardanoConverters;

  private final AdaPotsRepository adaPotsRepository;

  @GetMapping("/circulating")
  @LogMessage
  @Operation(
      summary = "Supply",
      description = "returns the ADA token circulating supply",
      tags = {"supply"})
  public ResponseEntity<Long> getSupplyCirculating() {
    AdaPots latestAdaPots = adaPotsRepository.findFirstByOrderByEpochNoDescSlotNoDesc();
    BigInteger circulatingSupply =
        CommonConstant.TOTAL_ADA
            .toBigInteger()
            .subtract(latestAdaPots.getReserves())
            .subtract(latestAdaPots.getTreasury());
    return ResponseEntity.ok(circulatingSupply.divide(BigInteger.valueOf(1_000_000L)).longValue());
  }

  @GetMapping("/total")
  @LogMessage
  @Operation(
      summary = "Supply",
      description = "returns the ADA token total supply",
      tags = {"supply"})
  public ResponseEntity<Long> getSupplyTotal() {
    AdaPots latestAdaPots = adaPotsRepository.findFirstByOrderByEpochNoDescSlotNoDesc();
    var supplyTotal = CommonConstant.TOTAL_ADA.toBigInteger().subtract(latestAdaPots.getReserves());
    return ResponseEntity.ok(supplyTotal.divide(BigInteger.valueOf(1_000_000L)).longValue());
  }
}
