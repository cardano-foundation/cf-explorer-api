package org.cardanofoundation.explorer.api.service;

public interface NewsService {

  /**
   * Get news data related to cardano
   * @param pageable pagination information
   * @return news data
   */
  Object getNews(Integer pageable);
}
