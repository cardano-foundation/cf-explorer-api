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
   * @param blockId block no or block hash
   * @return block information detail
   */
  BlockResponse getBlockDetailByBlockId(String blockId);

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
