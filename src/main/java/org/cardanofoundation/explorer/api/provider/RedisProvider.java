package org.cardanofoundation.explorer.api.provider;

import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import lombok.NonNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;

@Configuration
public class RedisProvider<K, V> {
  @Value("${application.network}")
  String network;

  @Autowired private RedisTemplate<K, V> redisTemplate;

  public V getValueByKey(@NonNull K key) {
    return redisTemplate.opsForValue().get(key);
  }

  public void setValueByKey(@NonNull K key, @NonNull V value) {
    redisTemplate.opsForValue().set(key, value);
  }

  public void setListValueByKey(@NonNull K key, long size, @NonNull V value) {
    redisTemplate.opsForList().set(key, size, value);
  }

  public void setValueByKey(@NonNull K key, @NonNull V value, long timeout, TimeUnit unit) {
    redisTemplate.opsForValue().set(key, value, timeout, unit);
  }

  public <HK, HV> void putAllHashByKey(
      @NonNull K key, @NonNull Map<? extends HK, ? extends HV> values) {
    redisTemplate.opsForHash().putAll(key, values);
  }

  public <H, HV> HV getHashValueByKey(@NonNull K key, Object hashKey) {
    return (HV) redisTemplate.opsForHash().get(key, hashKey);
  }

  public <HK, HV> java.util.List<HV> getSetHashValueByKey(@NonNull K key) {
    return (List<HV>) redisTemplate.opsForHash().values(key);
  }

  public <HK, HV> void putHashValueByKey(@NonNull K key, @NonNull HK hashValue, @NonNull HV value) {
    redisTemplate.opsForHash().put(key, hashValue, value);
  }

  public Boolean putHashIfAbsentByKey(@NonNull K key, Object hashKey, Object value) {
    return redisTemplate.opsForHash().putIfAbsent(key, hashKey, value);
  }

  public Boolean setExpire(@NonNull K key, Long timeout, TimeUnit timeUnit) {
    return redisTemplate.expire(key, timeout, timeUnit);
  }

  public Boolean setExpire(@NonNull K key, Duration timeout) {
    return redisTemplate.expire(key, timeout);
  }

  public <HV> List<HV> getListValueByKeys(@NonNull K key, Collection<Object> hashKeys) {
    return (List<HV>) redisTemplate.opsForHash().multiGet(key, hashKeys);
  }

  public Long getSizeListByKey(@NonNull K key) {
    return redisTemplate.opsForList().size(key);
  }

  public Long getSizeHashByKey(@NonNull K key) {
    return redisTemplate.opsForHash().size(key);
  }

  public List<V> getRangeListByKey(@NonNull K key, long start, long end) {
    return redisTemplate.opsForList().range(key, start, end);
  }

  public V rightPopListByKey(@NonNull K key) {
    return redisTemplate.opsForList().rightPop(key);
  }

  public Long leftPushListByKey(@NonNull K key, @NonNull V value) {
    return redisTemplate.opsForList().leftPush(key, value);
  }

  public Set<K> keys(@NonNull K keys) {
    return redisTemplate.keys(keys);
  }

  public void del(@NonNull Set<K> keys) {
    redisTemplate.delete(keys);
  }

  public Boolean hasKey(@NonNull K key) {
    return redisTemplate.hasKey(key);
  }

  public boolean del(@NonNull K key) {
    return Boolean.TRUE.equals(redisTemplate.delete(key));
  }

  public String getRedisKey(String prefix) {
    return (prefix + "_" + network).toUpperCase();
  }
}
