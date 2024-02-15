package org.cardanofoundation.explorer.api.event.blocksync;

import java.util.function.Consumer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import org.postgresql.PGNotification;

import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.model.redis.BlockSyncMessage;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;

@Component
@Slf4j
@RequiredArgsConstructor
public class BlockNotificationHandler implements Consumer<PGNotification> {

  private final ApplicationEventPublisher applicationEventPublisher;
  private final WebSocketService webSocketService;

  private final BlockRepository blockRepository;

  @Override
  public void accept(PGNotification t) {
    log.info(
        "Notification received: pid={}, name={}, param={}",
        t.getPID(),
        t.getName(),
        t.getParameter());
    String hash = t.getParameter();
    Block currentBlock = blockRepository.findCurrentBlockById();

    if (!hash.equals(currentBlock.getHash())) {
      return;
    }

    BlockSyncMessage blockSyncMessage =
        BlockSyncMessage.builder()
            .lastBlockNo(currentBlock.getBlockNo())
            .lastBlockHash(currentBlock.getHash())
            .hasTx(currentBlock.getTxCount() > 0)
            .build();

    WebSocketMessage webSocketMessage = webSocketService.getBatchBlockInfoMessage(blockSyncMessage);
    applicationEventPublisher.publishEvent(new WebSocketEvent(webSocketMessage) {});
  }
}
