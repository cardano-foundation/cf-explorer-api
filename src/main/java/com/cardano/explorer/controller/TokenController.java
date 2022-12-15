package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.service.TokenService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.SortDefault;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/tokens")
@RequiredArgsConstructor
public class TokenController {
  private final TokenService tokenService;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter token")
  public BaseFilterResponse<TokenFilterResponse> filter(
      @ParameterObject @SortDefault(sort = {"supply"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return tokenService.filterToken(pageable);
  }

  @GetMapping("/{tokenId}")
  @LogMessage
  @Operation(summary = "Detail token")
  public TokenResponse getTokenDetail(@PathVariable String tokenId) {
    return tokenService.getTokenDetail(tokenId);
  }

  @GetMapping("/{tokenId}/mints")
  @LogMessage
  @Operation(summary = "Filter token mint transaction")
  public BaseFilterResponse<TokenMintTxResponse> getTokenMintTx(@PathVariable String tokenId,
      @ParameterObject @SortDefault(sort = {"id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return tokenService.getMintTxs(tokenId, pageable);
  }

  @GetMapping("/{tokenId}/top_holders")
  @LogMessage
  @Operation(summary = "Filter holders of token")
  public BaseFilterResponse<TokenAddressResponse> getTopHolders(@PathVariable String tokenId,
      @ParameterObject Pageable pageable) {
    return tokenService.getTopHolders(tokenId, pageable);
  }
}
