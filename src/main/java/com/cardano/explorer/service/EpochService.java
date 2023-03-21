package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.EpochResponse;
import com.cardano.explorer.model.response.dashboard.EpochSummary;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

public interface EpochService {


  /**
   * Get epoch information detail by epoch no
   *
   * @param no number of epoch
   * @return epoch information detail
   */
  EpochResponse getEpochDetail(String no);

  /**
   * Get epoch list with paging
   *
   * @param pageable Page information
   * @return Epoch List in this page
   */
  BaseFilterResponse<EpochResponse> getAllEpoch(Pageable pageable);

  @Transactional(readOnly = true)
  EpochSummary getCurrentEpochSummary();
}
