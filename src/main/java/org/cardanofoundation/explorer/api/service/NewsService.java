package org.cardanofoundation.explorer.api.service;

public interface NewsService {

  /**
   * Get news data related to cardano
   * @param limit number of news in one page
   * @param offset offset of news in list
   * @return news data
   */
  Object getNews(Integer limit, Integer offset);
}
