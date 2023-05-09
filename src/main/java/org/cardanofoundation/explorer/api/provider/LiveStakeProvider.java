package org.cardanofoundation.explorer.api.provider;

import java.math.BigInteger;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.WithdrawalRepository;
import org.springframework.beans.factory.annotation.Value;
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

  @Value("${application.network}")
  private String network;

  public void calculateLiveStake() {
    log.info("start calculate live stake for pool...");
    List<String> poolViews = poolHashRepository.findAllView();
    BigInteger totalLiveStake = BigInteger.ZERO;
    for (String view : poolViews) {
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
      totalLiveStake = totalLiveStake.add(liveStake);
      redisTemplate.opsForValue().set(CommonConstant.REDIS_POOL_PREFIX + network + view, liveStake);
    }
    redisTemplate.opsForValue()
        .set(CommonConstant.REDIS_TOTAL_LIVE_STAKE + network, totalLiveStake);
    log.info("...end...");
  }
}
