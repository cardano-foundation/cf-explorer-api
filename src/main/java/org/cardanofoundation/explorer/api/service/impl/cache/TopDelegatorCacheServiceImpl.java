package org.cardanofoundation.explorer.api.service.impl.cache;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.enumeration.RedisKey;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.service.cache.TopDelegatorCacheService;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class TopDelegatorCacheServiceImpl implements TopDelegatorCacheService {

  private static final int MAX_ELEMENT_CACHE = 500;
  private static final int FIVE_MINUTES_IN_MILLISECONDS = 5 * 60 * 1000;
  private static final String COLON = ":";
  private static final Gson gson = new Gson();

  private final RedisTemplate<String, Object> redisTemplate;
  private final StakeAddressRepository stakeAddressRepository;

  @Value("${application.network}")
  String network;

  //todo: move this to scheduler-service
  @Scheduled(fixedDelay = FIVE_MINUTES_IN_MILLISECONDS)
  public void buildTopStakeDelegatorCache() {
    String redisKey = RedisKey.REDIS_TOP_STAKE_DELEGATORS.name() + COLON + network;
    long start = System.currentTimeMillis();
    Pageable pageable = PageRequest.of(0, MAX_ELEMENT_CACHE);
    var stakeAddressProjections = stakeAddressRepository.findStakeAddressOrderByBalance(pageable);
    var stakeIds = StreamUtil.mapApply(stakeAddressProjections, StakeAddressProjection::getId);

    redisTemplate.opsForValue().set(redisKey, gson.toJson(stakeIds));
    log.info("Build top-stake-delegators cache successfully, takes: [{} ms]", (System.currentTimeMillis() - start));
  }

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
