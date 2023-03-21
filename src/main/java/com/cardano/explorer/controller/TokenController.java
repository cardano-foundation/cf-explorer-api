package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.service.TokenService;
import com.cardano.explorer.service.TxService;
import com.sotatek.cardano.common.entity.BaseEntity_;
import com.sotatek.cardano.common.entity.MultiAsset_;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
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
          MultiAsset_.TX_COUNT}, direction = Sort.Direction.DESC) Pageable pageable) {
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
}
