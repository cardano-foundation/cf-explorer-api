package org.cardanofoundation.explorer.api.service.impl.cache;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.enumeration.RedisKey;
import org.cardanofoundation.explorer.api.service.cache.TopDelegatorCacheService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class TopDelegatorCacheServiceImpl implements TopDelegatorCacheService {

  private static final String COLON = ":";
  private static final Gson gson = new Gson();

  private final RedisTemplate<String, Object> redisTemplate;

  @Value("${application.network}")
  String network;

  public List<Long> getTopStakeDelegatorCache() {
    String redisKey = RedisKey.REDIS_TOP_STAKE_DELEGATORS.name() + COLON + network;
    Object topStakeIds = redisTemplate.opsForValue().get(redisKey);
    if (topStakeIds == null) {
      return Collections.emptyList();
    }
    try {
      return gson.fromJson(topStakeIds.toString(), new TypeToken<ArrayList<Long>>() {
      }.getType());
    } catch (Exception e) {
      log.error("Exception when get TopStakeDelegatorCache:", e);
      return Collections.emptyList();
    }
  }

}
