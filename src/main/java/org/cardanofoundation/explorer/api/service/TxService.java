package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

public interface TxService {

  List<TxSummary> findLatestTxSummary();

  List<TxGraph> getTransactionChartByRange(TxChartRange range);

  /**
   * Get list transaction with paging
   *
   * @param pageable page information
   * @return list transaction information in this page
   */
  BaseFilterResponse<TxFilterResponse> getAll(Pageable pageable);

  /**
   * Get transaction information detail by hash
   *
   * @param hash hash value of transaction
   * @return transaction detail
   */
  TxResponse getTxDetailByHash(String hash);

  /**
   * Get list transaction by blockId with paging
   *
   * @param blockId block no or block hash
   * @param pageable page information
   * @return list transaction information of this block in this page
   */
  BaseFilterResponse<TxFilterResponse> getTransactionsByBlock(String blockId, Pageable pageable);

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

  /**
   *
   * @param stakeKey stake key view value
   * @param pageable page information
   * @return list transaction of stake in this page
   */
  BaseFilterResponse<TxFilterResponse> getTransactionsByStake(String stakeKey, Pageable pageable);
}
