package org.cardanofoundation.explorer.api.service.impl.cache;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

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
public class RedisAggregatedDateCacheServiceImpl implements AggregatedDataCacheService {

  private static final String AGGREGATED_CACHE_KEY = "AGGREGATED_CACHE";
  private static final String BLOCK_TIME_HASH_KEY = "LATEST_BLOCK_TIME";
  private static final String TOKEN_COUNT_HASH_KEY = "TOTAL_TOKEN_COUNT";
  private final RedisTemplate<String, String> redisTemplate;

  private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(
      "yyyy-MM-dd HH:mm:ss");

  @Value("${application.network}")

  private String network;

  @Override
  public int getTokenCount() {
    String redisValue = getRedisHashValue(TOKEN_COUNT_HASH_KEY);
    return StringUtils.hasText(redisValue) ? Integer.parseInt(redisValue) : 0;
  }

  @Override
  public LocalDateTime getLatestBlockTime() {
    String blockTime = getRedisHashValue(BLOCK_TIME_HASH_KEY);
    if (Objects.isNull(blockTime)) {
      return null;
    }
    return LocalDateTime.parse(blockTime, DATE_TIME_FORMATTER);
  }

  private String getRedisHashValue(String hashKey) {
    HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();

    String redisKey = getRedisKey(AGGREGATED_CACHE_KEY);
    return hashOperations.get(redisKey, hashKey);
  }

  private String getRedisKey(String key) {
    return String.join(CommonConstant.UNDERSCORE, network.toUpperCase(), key);
  }

}
