package org.cardanofoundation.explorer.api.repository.cache;

import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.stereotype.Repository;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

@Repository
public class NativeScriptTopMultiAssetsCacheRepository {
  private final ValueOperations<String, TokenFilterResponse[]> operations;

  private final RedisTemplate<String, TokenFilterResponse[]> redisTemplate;

  @Value("${application.network}")
  private String network;

  private static final String NATIVE_SCRIPT_TOP_MULTI_ASSETS = "NATIVE_SCRIPT_TOP_MULTI_ASSETS";
  private static final long EXPIRE_TIME = 30 * 1000L;

  public NativeScriptTopMultiAssetsCacheRepository(RedisTemplate<String, TokenFilterResponse[]> redisTemplate) {
    this.redisTemplate = redisTemplate;
    this.operations = redisTemplate.opsForValue();
  }

  public List<TokenFilterResponse> findByScriptHash(String scriptHash) {
    TokenFilterResponse[] responses = operations.get(this.getRedisKey(scriptHash));
    return Objects.nonNull(responses) ? List.of(responses) : Collections.emptyList();
  }

  public void save(String scriptHash, List<TokenFilterResponse> tokenFilterResponses) {
    operations.set(this.getRedisKey(scriptHash), tokenFilterResponses.toArray(new TokenFilterResponse[0]));
    redisTemplate.expire(this.getRedisKey(scriptHash), Duration.ofMillis(EXPIRE_TIME));
  }

  private String getRedisKey(String key) {
    return NATIVE_SCRIPT_TOP_MULTI_ASSETS + "_" + network.toUpperCase() + ":" + key;
  }


}
