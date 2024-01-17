package org.cardanofoundation.explorer.api.service;

import reactor.core.publisher.Mono;

public interface WebClientService {
  public <T> Mono<T> callWebClient(String url, Class<T> clazz, Object... vars);
  public  <T> Mono<T> postWebClient(String url, Class<T> clazz, Object... vars);
}
