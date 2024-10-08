package org.cardanofoundation.explorer.api.service;

import java.util.List;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.BlockPropagationChartType;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockPropagationResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;

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
   * @return list block information in this page
   */
  BaseFilterResponse<BlockFilterResponse> filterBlock(Pageable pageable);

  /**
   * Get list block by epoch with paging
   *
   * @param no epoch number
   * @param pageable page information
   * @return list block of this epoch
   */
  BaseFilterResponse<BlockFilterResponse> getBlockByEpoch(String no, Pageable pageable);

  List<BlockPropagationResponse> getBlockPropagation(
      BlockPropagationChartType type, Pageable pageable);
}
