package org.cardanofoundation.explorer.api.event.blocksync;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class BlockEventListener implements MessageListener {

  private final ApplicationEventPublisher applicationEventPublisher;
  private final WebSocketService webSocketService;
  private final ObjectMapper objectMapper;

  /**
   * Callback for processing received objects through Redis. publish event to WebSocketEventHandler.
   * {@link org.cardanofoundation.explorer.api.event.websocket.WebSocketEventHandler}
   *
   * @param payload message must not be {@literal null}.
   * @param pattern pattern matching the channel (if specified) - can be {@literal null}.
   */
  @Override
  public void onMessage(Message payload, byte[] pattern) {
    try {
      BlockSyncMessage blockSyncMessage =
          objectMapper.readValue(payload.getBody(), BlockSyncMessage.class);
      log.info(
          "Received block sync message. Block hash: {}, Block no: {}",
          blockSyncMessage.getLastBlockHash(),
          blockSyncMessage.getLastBlockNo());
      WebSocketMessage webSocketMessage =
          webSocketService.getBatchBlockInfoMessage(blockSyncMessage);
      applicationEventPublisher.publishEvent(new WebSocketEvent(webSocketMessage) {});
    } catch (IOException e) {
      log.error("Error parsing block sync message: {}", e.getMessage());
    }
  }
}
