package org.cardanofoundation.explorer.api.service.impl.cache;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;

@Slf4j
@Service
@RequiredArgsConstructor
public class AggregatedDataCacheServiceImpl implements AggregatedDataCacheService {

  private static final String TEMP_PREFIX = "TEMP_";
  private static final String AGGREGATED_CACHE_KEY = "AGGREGATED_CACHE";
  private static final String TOKEN_COUNT_HASH_KEY = "TOTAL_TOKEN_COUNT";
  private final RedisTemplate<String, String> redisTemplate;

  @Value("${application.network}")
  private String network;

  @Override
  public int getTokenCount() {
    String redisValue = getRedisHashValue(TOKEN_COUNT_HASH_KEY);
    return StringUtils.hasText(redisValue) ? Integer.parseInt(redisValue) : 0;
  }

  private String getRedisHashValue(String hashKey) {
    HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();

    // Get from both temp and real data
    String tempRedisKey = getRedisKey(getTempRedisKey(AGGREGATED_CACHE_KEY));
    String data = hashOperations.get(tempRedisKey, hashKey);
    if (StringUtils.hasText(data)) {
      return data;
    }

    String redisKey = getRedisKey(AGGREGATED_CACHE_KEY);
    return hashOperations.get(redisKey, hashKey);
  }

  private String getRedisKey(String key) {
    return String.join(CommonConstant.UNDERSCORE, network.toUpperCase(), key);
  }

  private String getTempRedisKey(String key) {
    return TEMP_PREFIX + key;
  }
}
