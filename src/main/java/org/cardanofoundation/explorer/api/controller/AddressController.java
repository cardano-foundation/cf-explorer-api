package org.cardanofoundation.explorer.api.controller;

import java.util.concurrent.ExecutionException;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
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
import org.cardanofoundation.explorer.common.pagination.Pagination;
import org.cardanofoundation.explorer.common.pagination.PaginationCondition;
import org.cardanofoundation.explorer.common.pagination.PaginationDefault;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/addresses")
@RequiredArgsConstructor
@Validated
public class AddressController {

  private final AddressService addressService;

  private final TxService txService;

  @GetMapping("/{address}")
  @LogMessage
  @Operation(summary = "Get a address detail")
  public ResponseEntity<AddressResponse> getAddressDetail(
      @PathVariable @Parameter(description = "Address") String address) {
    return ResponseEntity.ok(addressService.getAddressDetail(address));
  }

  @GetMapping("/top-addresses")
  @LogMessage
  @Operation(summary = "Get top addresses")
  public ResponseEntity<BaseFilterResponse<AddressFilterResponse>> getTopAddress(
          @ParameterObject @PaginationCondition(shouldValidateSort = true, sort={"id", "time", "index"})
          @PaginationDefault(size = 20, sort = {
                  "id"}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(addressService.getTopAddress(pagination.toPageable()));
  }

  @GetMapping("/analytics/{address}/{type}")
  @LogMessage
  @Operation(summary = "Get a address analytics")
  public ResponseEntity<List<AddressAnalyticsResponse>> getAddressAnalytics(
      @PathVariable @Parameter(description = "Address") String address,
      @PathVariable @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type)
      throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(addressService.getAddressAnalytics(address, type));
  }

  @GetMapping("/min-max-balance/{address}")
  @LogMessage
  @Operation(summary = "Get the highest and lowest balance address")
  public ResponseEntity<List<BigInteger>> getAddressMinMaxBalance(@PathVariable String address) {
    return ResponseEntity.ok(addressService.getAddressMinMaxBalance(address));
  }

  @GetMapping("/{address}/txs")
  @LogMessage
  @Operation(summary = "Get the highest and lowest balance address")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(@PathVariable String address,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(txService.getTransactionsByAddress(address, pageable));
  }

  @GetMapping("/{address}/tokens")
  @LogMessage
  @Operation(summary = "Get list token by address")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTokenByAddress(
      @PathVariable String address,
      @RequestParam(required = false) String displayName,
      @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(addressService.getTokenByDisplayName(pageable, address, displayName));
  }
}
