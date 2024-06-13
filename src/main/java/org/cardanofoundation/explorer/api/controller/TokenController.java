package org.cardanofoundation.explorer.api.controller;

import java.util.List;
import java.util.concurrent.ExecutionException;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.entity.ledgersync.BaseEntity_;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxAmount_;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.LatestTokenBalance_;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;

@RestController
@RequestMapping("/api/v1/tokens")
@RequiredArgsConstructor
@Validated
@Tag(name = "tokens", description = "The token APIs")
public class TokenController {
  private final TokenService tokenService;
  private final TxService txService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter token")
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> filter(
      @ParameterObject @PaginationValid @Valid Pagination pagination,
      @Parameter(description = "Token name") @RequestParam(required = false) String query)
      throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(tokenService.filterToken(query, pagination.toPageable()));
  }

  @GetMapping("/{tokenId}")
  @LogMessage
  @Operation(summary = "Detail token")
  public ResponseEntity<TokenResponse> getTokenDetail(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
          @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH)
          @Parameter(description = "The CIP14 fingerprint for the MultiAsset.")
          String tokenId) {
    return ResponseEntity.ok(tokenService.getTokenDetail(tokenId));
  }

  @GetMapping("/{tokenId}/mints")
  @LogMessage
  @Operation(summary = "Filter token mint transaction")
  public ResponseEntity<BaseFilterResponse<TokenMintTxResponse>> getTokenMintTx(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
          @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH)
          String tokenId,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              sort = {BaseEntity_.ID},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(tokenService.getMintTxs(tokenId, pagination.toPageable()));
  }

  @GetMapping("/{tokenId}/top_holders")
  @LogMessage
  @Operation(summary = "Filter holders by token")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTopHolders(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
          @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH)
          @Parameter(description = "The CIP14 fingerprint for the MultiAsset.")
          String tokenId,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              sort = {LatestTokenBalance_.QUANTITY},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(tokenService.getTopHolders(tokenId, pagination.toPageable()));
  }

  @GetMapping("/{tokenId}/txs")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
          @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH)
          @Parameter(description = "The CIP14 fingerprint for the MultiAsset.")
          String tokenId,
      @ParameterObject
          @PaginationValid
          @PaginationDefault(
              size = 20,
              sort = {AddressTxAmount_.BLOCK_TIME},
              direction = Sort.Direction.DESC)
          @Valid
          Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByToken(tokenId, pagination.toPageable()));
  }

  @GetMapping("/analytics/{tokenId}/{type}")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<List<TokenVolumeAnalyticsResponse>> getTokenVolumeAnalytics(
      @PathVariable
          @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
          @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH)
          @Parameter(description = "The CIP14 fingerprint for the MultiAsset.")
          String tokenId,
      @PathVariable @Parameter(description = "Type analytics") AnalyticType type) {
    return ResponseEntity.ok(tokenService.getTokenVolumeAnalytic(tokenId, type));
  }
}
