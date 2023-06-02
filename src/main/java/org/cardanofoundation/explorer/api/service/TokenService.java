package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenVolumeAnalyticsResponse;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.springframework.data.domain.Pageable;

public interface TokenService {

  /**
   * Get list token with paging
   *
   * @param pageable page information
   * @return list tokens information in this page
   */
  BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) throws ExecutionException, InterruptedException;

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
  List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId, AnalyticType type) throws ExecutionException, InterruptedException;
}