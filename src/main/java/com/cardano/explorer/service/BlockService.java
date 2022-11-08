package com.cardano.explorer.service;

import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.BlockFilterResponse;
import com.cardano.explorer.model.BlockResponse;
import org.springframework.data.domain.Pageable;

public interface BlockService {

  /**
   * Get information detail of block having block no = :no
   *
   * @param no block no
   * @return block information detail
   */
  BlockResponse getBlockDetail(Integer no);

  /**
   * Get list block with paging
   *
   * @param pageable page information
   * @return list block information in this page
   */
  BaseFilterResponse<BlockFilterResponse> getAllBlock(Pageable pageable);

  /**
   * Get information of block having epoch no
   *
   * @param epochNo  epoch no
   * @param pageable page information
   * @return list block information in this page
   */
  BaseFilterResponse<BlockFilterResponse> getBlockByEpochNo(Integer epochNo, Pageable pageable);
}
