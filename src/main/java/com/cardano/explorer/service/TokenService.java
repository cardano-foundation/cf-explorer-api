package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import org.springframework.data.domain.Pageable;

public interface TokenService {

  /**
   * Get list token with paging
   *
   * @param pageable page information
   * @return list tokens information in this page
   */
  BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable);

  /**
   * Get token detail by token id (fingerprint)
   *
   * @param tokenId token id (fingerprint)
   * @return token detail info
   */
  TokenResponse getTokenDetail(String tokenId);

  /**
   * Get minting transaction by token
   *
   * @param tokenId token id (fingerprint)
   * @param pageable page information
   * @return list minting transactions of token in this page
   */
  BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable);

  /**
   *
   * Get top holders  by token
   *
   * @param tokenId token id (fingerprint)
   * @param pageable page information
   * @return list addresses hold token in this page
   */
  BaseFilterResponse<TokenAddressResponse> getTopHolders(String tokenId, Pageable pageable);
}