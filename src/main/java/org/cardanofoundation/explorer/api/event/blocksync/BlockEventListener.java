package org.cardanofoundation.explorer.api.event.blocksync;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class BlockEventListener implements MessageListener {

  private final ApplicationEventPublisher applicationEventPublisher;
  private final EpochService epochService;

  /**
   * Callback for processing received objects through Redis. publish event to WebSocketEventHandler.
   * {@link org.cardanofoundation.explorer.api.event.websocket.WebSocketEventHandler}
   *
   * @param payload message must not be {@literal null}.
   * @param pattern pattern matching the channel (if specified) - can be {@literal null}.
   */
  @Override
  public void onMessage(Message payload, byte[] pattern) {
    long blockNo = Long.parseLong(new String(payload.getBody()));
    log.info("Received new block no: {}", blockNo);
    BlockSyncInfo blockSyncInfo =
        BlockSyncInfo.builder()
            .blockNo(blockNo)
            .epochSummary(epochService.getCurrentEpochSummary())
            .build();
    WebSocketMessage webSocketMessage =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.BLOCK)
            .payload(blockSyncInfo)
            .build();
    applicationEventPublisher.publishEvent(new WebSocketEvent(webSocketMessage) {});
  }
}
