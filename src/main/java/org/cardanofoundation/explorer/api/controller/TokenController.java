package org.cardanofoundation.explorer.api.controller;

import java.util.concurrent.ExecutionException;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.common.validation.prefixed.PrefixedValid;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset_;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/tokens")
@RequiredArgsConstructor
@Validated
public class TokenController {
  private final TokenService tokenService;
  private final TxService txService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter token")
  public ResponseEntity<BaseFilterResponse<TokenFilterResponse>> filter(
       @ParameterObject @PaginationValid @PaginationDefault(sort = {MultiAsset_.SUPPLY,
          MultiAsset_.TX_COUNT}, direction = Sort.Direction.DESC) Pagination pagination)
          throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(tokenService.filterToken(pagination.toPageable()));
  }

  @GetMapping("/{tokenId}")
  @LogMessage
  @Operation(summary = "Detail token")
  public ResponseEntity<TokenResponse> getTokenDetail(@PathVariable @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
       @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH) String tokenId) {
    return ResponseEntity.ok(tokenService.getTokenDetail(tokenId));
  }

  @GetMapping("/{tokenId}/mints")
  @LogMessage
  @Operation(summary = "Filter token mint transaction")
  public ResponseEntity<BaseFilterResponse<TokenMintTxResponse>> getTokenMintTx(
       @PathVariable @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
       @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH) String tokenId,
       @PaginationValid @PaginationDefault(sort = {
          BaseEntity_.ID}, direction = Sort.Direction.DESC) Pagination pagination) {
    return ResponseEntity.ok(tokenService.getMintTxs(tokenId, pagination.toPageable()));
  }

  @GetMapping("/{tokenId}/top_holders")
  @LogMessage
  @Operation(summary = "Filter holders by token")
  public ResponseEntity<BaseFilterResponse<TokenAddressResponse>> getTopHolders(
       @PathVariable @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
       @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH) String tokenId,
       @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(tokenService.getTopHolders(tokenId, pagination.toPageable()));
  }

  @GetMapping("/{tokenId}/txs")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> getTransactions(
       @PathVariable @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
       @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH) String tokenId,
       @ParameterObject @PaginationValid Pagination pagination) {
    return ResponseEntity.ok(txService.getTransactionsByToken(tokenId, pagination.toPageable()));
  }

  @GetMapping("/analytics/{tokenId}/{type}")
  @LogMessage
  @Operation(summary = "Filter transaction by token")
  public ResponseEntity<List<TokenVolumeAnalyticsResponse>> getTokenVolumeAnalytics(
       @PathVariable @PrefixedValid(CommonConstant.PREFIXED_TOKEN_FINGERPRINT)
       @LengthValid(CommonConstant.TOKEN_FINGERPRINT_LENGTH) String tokenId, @PathVariable
       @Parameter(description = "Type analytics: 1d, 1w, 1m, 3m") AnalyticType type)
       throws ExecutionException, InterruptedException {
    return ResponseEntity.ok(tokenService.getTokenVolumeAnalytic(tokenId, type));
  }
}