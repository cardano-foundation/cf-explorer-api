package com.cardano.explorer.service;

import com.cardano.explorer.model.request.BlockFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import org.springframework.data.domain.Pageable;

public interface BlockService {

  /**
   * Get information detail by block no
   *
   * @param no block no
   * @return block information detail
   */
  BlockResponse getBlockDetailByBlockNo(String no);

  /**
   * Get information detail by block hash
   *
   * @param hash block hash
   * @return block information detail
   */
  BlockResponse getBlockDetailByHash(String hash);

  /**
   * Get list block with paging
   *
   * @param pageable page information
   * @param request request condition
   * @return list block information in this page
   */
  BaseFilterResponse<BlockFilterResponse> filterBlock(Pageable pageable,
      BlockFilterRequest request);
}
