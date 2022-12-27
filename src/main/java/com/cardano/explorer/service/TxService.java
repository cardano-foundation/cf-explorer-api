package com.cardano.explorer.service;

import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.cardano.explorer.model.response.dashboard.TxGraph;
import com.cardano.explorer.model.response.dashboard.TxSummary;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

public interface TxService {

  @Transactional(readOnly = true)
  List<TxSummary> findLatestTxSummary();

  @Transactional(readOnly = true)
  List<TxGraph> getTxsAfterTime();

  /**
   * Get list transaction with paging
   *
   * @param pageable page information
   * @param request  request condition
   * @return list transaction information in this page
   */
  BaseFilterResponse<TxFilterResponse> filterTx(Pageable pageable, TxFilterRequest request);

  /**
   * Get transaction information detail by hash
   *
   * @param hash hash value of transaction
   * @return transaction detail
   */
  TxResponse getTxDetailByHash(String hash);


  /**
   * Get list transaction having address
   *
   * @param address wallet address
   * @param pageable page information
   * @return  list transaction information in this page
   */
  BaseFilterResponse<TxFilterResponse> getTransactionsByAddress(String address, Pageable pageable);


  /** Get transaction by token
   * @param tokenId fingerprint of token
   * @param pageable page information
   * @return list transaction of token in this page
   */
  BaseFilterResponse<TxFilterResponse> getTransactionsByToken(String tokenId, Pageable pageable);
}
