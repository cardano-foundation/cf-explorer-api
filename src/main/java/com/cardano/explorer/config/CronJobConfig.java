package com.cardano.explorer.config;

import com.cardano.explorer.provider.LiveStakeProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CronJobConfig {

  private final LiveStakeProvider liveStakeProvider;

  @Scheduled(cron = "0 0 0 * * *")
  public void pushLiveStakeToRedis() {
    liveStakeProvider.calculateLiveStake();
  }
}
