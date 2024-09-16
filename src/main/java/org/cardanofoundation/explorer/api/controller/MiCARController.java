package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.micar.AddressCarbonEmissionResponse;
import org.cardanofoundation.explorer.api.service.MiCARService;

@RestController
@RequestMapping("/api/v1/MiCAR")
@RequiredArgsConstructor
@Validated
@Tag(name = "MiCar", description = "The MiCAR APIs")
public class MiCARController {
  private final MiCARService miCARService;

  @GetMapping("/carbon-emission/{address}")
  @LogMessage
  @Operation(
      summary = "Get the carbon emissions of the given addresses",
      description = "Get the carbon emissions of the given addresses",
      tags = {"MiCar"})
  public ResponseEntity<AddressCarbonEmissionResponse> getAddressCarbonEmissions(
      @PathVariable
          @Parameter(
              description =
                  " The address or stake address for which the carbon emissions are to be calculated.")
          String address) {
    return ResponseEntity.ok(miCARService.getCarbonEmissionsByAddressAndPool(address));
  }

  @GetMapping("/carbon-emission/overview")
  @LogMessage
  @Operation(
      summary = "Get the carbon emissions overview",
      description = "Get the carbon emissions overview",
      tags = {"MiCar"})
  public ResponseEntity<Object> getOverviewCarbonEmissions(
      @RequestParam String responseType, @RequestParam String key) {
    return ResponseEntity.ok(miCARService.getCarbonEmissionsOverview(responseType, key));
  }

  @GetMapping("/carbon-emission/historical")
  @LogMessage
  @Operation(
      summary = "Get the carbon emissions historical",
      description = "Get the carbon emissions historical",
      tags = {"MiCar"})
  public ResponseEntity<Object> getHistoricalCarbonEmissions(@RequestParam String key) {
    return ResponseEntity.ok(miCARService.getCarbonEmissionsHistorical(key));
  }
}
