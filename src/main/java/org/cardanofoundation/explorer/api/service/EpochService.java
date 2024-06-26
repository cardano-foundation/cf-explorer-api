package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;

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

  /**
   * Get latest epoch synced from the consumer. Display basic epoch information (epoch no, used slot
   * ,length, total unique account).
   *
   * <p>In first time this function will get total unique accounts (stake key, byron address) in
   * database. Then insert unique accounts to cache and transaction id check point After that this
   * function will find in cache and update from the transaction check point
   *
   * @return epoch summary
   */
  EpochSummary getCurrentEpochSummary();
}
