package org.cardanofoundation.explorer.api.controller;

import java.util.concurrent.ExecutionException;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.controller.validation.PageZeroValid;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.service.TxService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.math.BigInteger;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
      description = "Get detail information of payment address with balance, txs, token and checjk contract",
      tags = {"address"})
  public ResponseEntity<AddressResponse> getAddressDetail(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address) {
    return ResponseEntity.ok(addressService.getAddressDetail(address));
  }

  @GetMapping("/top-addresses")
  @LogMessage
  @Operation(
      summary = "Get top address by balance",
      tags = {"address"})
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getTopAddress(
      @ParameterObject @PaginationValid @PageZeroValid Pagination pagination) {
    return ResponseEntity.ok(addressService.getTopAddress(pagination.toPageable()));
  }

  @GetMapping("/analytics/{address}/{type}")
  @LogMessage
  @Operation(
      summary = "Get analytics of address",
      description = "Get analytics balance of address and time type",
      tags = {"address"})
  public ResponseEntity<List<AddressAnalyticsResponse>> getAddressAnalytics(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address,
      @PathVariable @Parameter(description = "Type for analytics by time") AnalyticType type)
      throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(addressService.getAddressAnalytics(address, type));
  }

  @GetMapping("/min-max-balance/{address}")
  @LogMessage
  @Operation(
      summary = "Get the highest and lowest balance of address",
      tags = {"address"})
  public ResponseEntity<List<BigInteger>> getAddressMinMaxBalance(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address) {
    return ResponseEntity.ok(addressService.getAddressMinMaxBalance(address));
  }

  @GetMapping("/{address}/txs")
  @LogMessage
  @Operation(
      summary = "Get list transaction by address",
      tags = {"address"})
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByAddress(address, pagination.toPageable()));
  }

  @GetMapping("/{address}/tokens")
  @LogMessage
  @Operation(
      summary = "Get list token by address",
      description = "Get list token by address with search by display name, will return all token if display name is null or empty",
      tags = {"address"})
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTokenByAddress(
      @PathVariable @Parameter(description = "The human readable encoding of the output address."
          + " Will be Base58 for Byron era addresses and Bech32 for Shelley era.") String address,
      @RequestParam(required = false) @Parameter(description = "Display name query for search") String displayName,
      @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(addressService.getTokenByDisplayName(pagination.toPageable(), address, displayName));
  }
}