package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.service.NewsService;
import org.cardanofoundation.explorer.api.service.WebClientService;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class NewsServiceImpl implements NewsService {

  @Value("${application.api.news.base-url}")
  private String apiNewsDataUrl;

  private final WebClientService webClientService;

  @SingletonCall(typeToken = TypeTokenGson.NEWS, expireAfterSeconds = 200)
  public Object getNews(Integer limit, Integer offset) {
    return webClientService.callWebClient(String.format(apiNewsDataUrl, limit, offset),Object.class).block();
  }
}