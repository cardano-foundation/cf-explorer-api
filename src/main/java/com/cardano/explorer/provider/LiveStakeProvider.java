package com.cardano.explorer.provider;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import java.math.BigInteger;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

@Component
@Log4j2
@RequiredArgsConstructor
public class LiveStakeProvider {

  private final DelegationRepository delegationRepository;

  private final RewardRepository rewardRepository;

  private final WithdrawalRepository withdrawalRepository;

  private final PoolHashRepository poolHashRepository;

  private final RedisTemplate<String, Object> redisTemplate;

  public void calculateLiveStake() {
    log.info("start calculate live stake for pool...");
    List<String> poolViews = poolHashRepository.findAllView();
    BigInteger totalLiveStake = BigInteger.ZERO;
    poolViews.forEach(view -> {
      BigInteger delegateStake = delegationRepository.findDelegateStakeByPool(view);
      BigInteger rewardStake = rewardRepository.findRewardStakeByPool(view);
      BigInteger withdrawalStake = withdrawalRepository.findWithdrawalStakeByPool(view);
      if (Objects.isNull(delegateStake)) {
        delegateStake = BigInteger.ZERO;
      }
      if (Objects.isNull(rewardStake)) {
        rewardStake = BigInteger.ZERO;
      }
      if (Objects.isNull(withdrawalStake)) {
        withdrawalStake = BigInteger.ZERO;
      }
      BigInteger liveStake = delegateStake.add(rewardStake).subtract(withdrawalStake);
      totalLiveStake.add(liveStake);
      redisTemplate.opsForValue().set(CommonConstant.REDIS_POOL_PREFIX + view, liveStake);
    });
    redisTemplate.opsForValue().set(CommonConstant.REDIS_TOTAL_LIVE_STAKE, totalLiveStake);
    log.info("...end...");
  }
}
