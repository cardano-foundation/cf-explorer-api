package com.cardano.explorer.service;

import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.TxFilterResponse;
import com.cardano.explorer.model.TxResponse;
import org.springframework.data.domain.Pageable;

public interface TxService {
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

}
