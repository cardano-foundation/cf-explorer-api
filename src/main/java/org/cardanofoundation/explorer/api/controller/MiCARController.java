package org.cardanofoundation.explorer.api.controller;

import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
}
