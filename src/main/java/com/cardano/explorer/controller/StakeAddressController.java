package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.service.StakeAddressService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/stakeKey")
@RequiredArgsConstructor
public class StakeAddressController {

  private final StakeAddressService stakeAddressService;
  @GetMapping("/{address}")
  @LogMessage
  @Operation(summary = "Get a stake address detail by wallet address")
  public StakeAddressResponse getAddressDetail(
      @PathVariable @Parameter(description = "Wallet Address") String address) {
    return stakeAddressService.getStakeAddress(address);
  }
}
