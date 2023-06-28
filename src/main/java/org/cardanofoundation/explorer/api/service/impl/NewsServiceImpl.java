package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.service.NewsService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@RequiredArgsConstructor
public class NewsServiceImpl implements NewsService {

  @Value("${application.api.news.base-url}")
  private String apiNewsDataUrl;

  private final RestTemplate restTemplate;

  @SingletonCall(typeToken = TypeTokenGson.NEWS, expireAfterSeconds = 200)
  public Object getNews(Integer amount) {
    return restTemplate.getForObject(String.format(apiNewsDataUrl, amount), Object.class);
  }
}