package com.cardano.explorer.service;

import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.EpochResponse;
import org.springframework.data.domain.Pageable;

public interface EpochService {


  /**
   *  Get epoch information detail by epoch no
   * @param no number of epoch
   * @return epoch information detail
   */
  EpochResponse getEpochDetail(Integer no);


  /**
   * Get current epoch no
   * @return current epoch no
   */
  Integer getCurrentEpoch();

  /**
   * Get epoch list with paging
   * @param pageable Page information
   * @return Epoch List in this page
   */
  BaseFilterResponse<EpochResponse> filterEpoch(Pageable pageable);
}
