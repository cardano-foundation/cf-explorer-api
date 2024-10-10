package org.cardanofoundation.explorer.api.controller;

import java.math.BigInteger;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@RestController
@RequestMapping("/api/v1/supply")
@RequiredArgsConstructor
@Tag(name = "supply", description = "ada token supply information")
public class SupplyController {

  private static final BigInteger ONE_ADA_IN_LOVELACES = BigInteger.valueOf(1_000_000L);

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
    return ResponseEntity.ok(circulatingSupply.divide(ONE_ADA_IN_LOVELACES).longValue());
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
    return ResponseEntity.ok(supplyTotal.divide(ONE_ADA_IN_LOVELACES).longValue());
  }
}
