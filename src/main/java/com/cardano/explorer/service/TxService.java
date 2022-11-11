package com.cardano.explorer.service;

import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxResponse;
import org.springframework.data.domain.Pageable;

public interface TxService {

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

}
