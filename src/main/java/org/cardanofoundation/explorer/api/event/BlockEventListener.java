package org.cardanofoundation.explorer.api.event;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class BlockEventListener implements MessageListener {

  private final ApplicationEventPublisher applicationEventPublisher;

  /**
   * Callback for processing received objects through Redis.
   *
   * @param payload message must not be {@literal null}.
   * @param pattern pattern matching the channel (if specified) - can be {@literal null}.
   */
  @Override
  public void onMessage(Message payload, byte[] pattern) {
    long blockNo = Long.parseLong(new String(payload.getBody()));
    log.info("Received new block no: {}", blockNo);
    applicationEventPublisher.publishEvent(new BlockSyncEvent(blockNo) {});
  }
}
