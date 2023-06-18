package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset_;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/tokens")
@RequiredArgsConstructor
public class TokenController {
  private final TokenService tokenService;
  private final TxService txService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter token")
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> filter(
      @ParameterObject @SortDefault(sort = {MultiAsset_.SUPPLY,
          MultiAsset_.TX_COUNT}, direction = Sort.Direction.DESC) Pageable pageable)
      throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(tokenService.filterToken(pageable));
  }

  @GetMapping("/{tokenId}")
  @LogMessage
  @Operation(summary = "Detail token")
  public ResponseEntity<TokenResponse> getTokenDetail(@PathVariable String tokenId) {
    return ResponseEntity.ok(tokenService.getTokenDetail(tokenId));
  }

  @GetMapping("/{tokenId}/mints")
  @LogMessage
  @Operation(summary = "Filter token mint transaction")
  public ResponseEntity<BaseFilterResponse<TokenMintTxResponse>> getTokenMintTx(
      @PathVariable String tokenId, @ParameterObject @SortDefault(sort = {
      BaseEntity_.ID}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(tokenService.getMintTxs(tokenId, pageable));
  }

  @GetMapping("/{tokenId}/top_holders")
  @LogMessage
  @Operation(summary = "Filter holders by token")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTopHolders(
      @PathVariable String tokenId, @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(tokenService.getTopHolders(tokenId, pageable));
  }

  @GetMapping("/{tokenId}/txs")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable String tokenId, @ParameterObject Pageable pageable) {
    return ResponseEntity.ok(txService.getTransactionsByToken(tokenId, pageable));
  }

  @GetMapping("/analytics/{tokenId}/{type}")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<List<TokenVolumeAnalyticsResponse>> getTokenVolumeAnalytics(
      @PathVariable String tokenId,
      @PathVariable @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type
  ) throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(tokenService.getTokenVolumeAnalytic(tokenId, type));
  }
}
