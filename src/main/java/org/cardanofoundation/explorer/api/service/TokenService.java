package org.cardanofoundation.explorer.api.service;

import java.util.concurrent.ExecutionException;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface TokenService {

  /**
   * Get list token with paging
   *
   * @param pageable page information
   * @return list tokens information in this page
   */
  BaseFilterResponse<TokenFilterResponse> filterToken(String query, Pageable pageable)
      throws ExecutionException, InterruptedException;

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

  /**
   * Analysis volume of token
   *
   * @param tokenId token id (fingerprint)
   * @param type type of analytic
   * @return list analytic volume of token
   */
  List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId, AnalyticType type);
}