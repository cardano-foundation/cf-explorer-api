package org.cardanofoundation.explorer.api.config;

import org.cardanofoundation.explorer.api.provider.LiveStakeProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CronJobConfig {

  private final LiveStakeProvider liveStakeProvider;

  @Scheduled(cron = "0 30 14 * * *")
  public void pushLiveStakeToRedis() {
    liveStakeProvider.calculateLiveStake();
  }
}
