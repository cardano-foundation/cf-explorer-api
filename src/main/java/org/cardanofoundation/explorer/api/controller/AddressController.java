package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestTokenBalance_;
import org.cardanofoundation.explorer.common.validation.pagination.PageZeroValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;

@RestController
@RequestMapping("/api/v1/addresses")
@RequiredArgsConstructor
@Validated
@Tag(name = "address", description = "The address APIs")
public class AddressController {

  private final AddressService addressService;

  private final TxService txService;

    @GetMapping("/{address}")
    @LogMessage
    @Operation(
        summary = "Get detail information of payment address",
        description =
            "Get detail information of payment address with balance, txs, token and check contract",
        tags = {"address"})
    public ResponseEntity<AddressResponse> getAddressDetail(
        @PathVariable
            @Parameter(
                description =
                    "The human readable encoding of the output address."
                        + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.")
            String address) {
      return ResponseEntity.ok(addressService.getAddressDetail(address));
    }

  @GetMapping("/top-addresses")
  @LogMessage
  @Operation(
      summary = "Get top address by balance",
      tags = {"address"})
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getTopAddress(
      @ParameterObject
      @PaginationDefault(
          size = 20,
          sort = {LatestTokenBalance_.QUANTITY},
          direction = Sort.Direction.DESC)
      @PaginationValid @PageZeroValid @Valid Pagination pagination) {
    return ResponseEntity.ok(addressService.getTopAddress(pagination.toPageable()));
  }

    @GetMapping("/analytics/{address}/{type}")
    @LogMessage
    @Operation(
        summary = "Get analytics of address",
        description = "Get analytics balance of address and time type",
        tags = {"address"})
    public ResponseEntity<AddressChartBalanceResponse> getAddressAnalytics(
        @PathVariable
            @Parameter(
                description =
                    "The human readable encoding of the output address."
                        + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.")
            String address,
        @PathVariable @Parameter(description = "Type for analytics by time") AnalyticType type) {
      return ResponseEntity.ok(addressService.getAddressAnalytics(address, type));
    }

  //  @GetMapping("/{address}/txs")
  //  @LogMessage
  //  @Operation(
  //      summary = "Get list transaction by address",
  //      tags = {"address"})
  //  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
  //      @PathVariable
  //          @Parameter(
  //              description =
  //                  "The human readable encoding of the output address."
  //                      + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.")
  //          String address,
  //      @ParameterObject @PaginationValid @Valid Pagination pagination) {
  //    return ResponseEntity.ok(txService.getTransactionsByAddress(address,
  // pagination.toPageable()));
  //  }

    @GetMapping("/{address}/tokens")
    @LogMessage
    @Operation(
        summary = "Get list token by address",
        description =
            "Get list token by address with search by display name, will return all token if display name is null or empty",
        tags = {"address"})
    public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTokenByAddress(
        @PathVariable
            @Parameter(
                description =
                    "The human readable encoding of the output address."
                        + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.")
            String address,
        @RequestParam(required = false) @Parameter(description = "Display name query for search")
            String displayName,
        @ParameterObject @PaginationValid @Valid Pagination pagination) {
      return ResponseEntity.ok(
          addressService.getTokenByDisplayName(pagination.toPageable(), address, displayName));
    }
}
