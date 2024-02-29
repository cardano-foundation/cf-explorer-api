package org.cardanofoundation.explorer.api.service.impl.cache;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import org.cardanofoundation.explorer.api.provider.RedisProvider;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;

@Slf4j
@Service
@RequiredArgsConstructor
public class RedisAggregatedDateCacheServiceImpl implements AggregatedDataCacheService {

  private static final String AGGREGATED_CACHE_KEY = "AGGREGATED_CACHE";
  private static final String BLOCK_TIME_HASH_KEY = "LATEST_BLOCK_TIME";
  private static final String TOKEN_COUNT_HASH_KEY = "TOTAL_TOKEN_COUNT";
  private static final String BLOCK_INSERT_TIME_HASH_KEY = "LATEST_BLOCK_INSERT_TIME";
  private final RedisProvider<String, String> redisProvider;

  private static final DateTimeFormatter DATE_TIME_FORMATTER =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  @Value("${application.network}")
  private String network;

  @Override
  public LocalDateTime getLatestBlockTime() {
    String blockTime = getRedisHashValue(BLOCK_TIME_HASH_KEY);
    if (Objects.isNull(blockTime)) {
      return null;
    }
    return LocalDateTime.parse(blockTime, DATE_TIME_FORMATTER);
  }

  @Override
  public LocalDateTime getLatestBlockInsertTime() {
    String blockTime = getRedisHashValue(BLOCK_INSERT_TIME_HASH_KEY);
    if (Objects.isNull(blockTime)) {
      return null;
    }
    return LocalDateTime.parse(blockTime, DATE_TIME_FORMATTER);
  }

  @Override
  public int getTokenCount() {
    String redisValue = getRedisHashValue(TOKEN_COUNT_HASH_KEY);
    return StringUtils.hasText(redisValue) ? Integer.parseInt(redisValue) : 0;
  }

  private String getRedisHashValue(String hashKey) {
    return redisProvider.getHashValueByKey(
        redisProvider.getRedisKey(AGGREGATED_CACHE_KEY), hashKey);
  }
}
