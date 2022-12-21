package com.cardano.explorer.controller;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.service.AddressService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.math.BigDecimal;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/address")
@RequiredArgsConstructor
public class AddressController {

  private final AddressService addressService;

  @GetMapping("/{address}")
  @LogMessage
  @Operation(summary = "Get a address detail")
  public AddressResponse getAddressDetail(
      @PathVariable @Parameter(description = "Address") String address) {
    return addressService.getAddressDetail(address);
  }

  @GetMapping("/analytics/{address}/{type}")
  @LogMessage
  @Operation(summary = "Get a address analytics")
  public List<AddressAnalyticsResponse> getAddressAnalytics(
      @PathVariable @Parameter(description = "Address") String address,
      @PathVariable @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type) {
    return addressService.getAddressAnalytics(address, type);
  }

  @GetMapping("/min-max-balance/{address}")
  @LogMessage
  @Operation(summary = "Get the highest and lowest balance address")
  public List<BigDecimal> getAddressMinMaxBalance(@PathVariable String address) {
    return addressService.getAddressMinMaxBalance(address);
  }
}
