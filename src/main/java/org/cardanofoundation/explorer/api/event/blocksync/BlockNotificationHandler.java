package org.cardanofoundation.explorer.api.event.blocksync;

import java.util.function.Consumer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import org.postgresql.PGNotification;

import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;

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
    Long blockId = Long.parseLong(t.getParameter());
    Block block =
        blockRepository
            .findById(blockId)
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));

    BlockSyncMessage blockSyncMessage =
        BlockSyncMessage.builder()
            .lastBlockNo(block.getBlockNo())
            .lastBlockHash(block.getHash())
            .hasTx(block.getTxCount() > 0)
            .build();

    WebSocketMessage webSocketMessage = webSocketService.getBatchBlockInfoMessage(blockSyncMessage);
    applicationEventPublisher.publishEvent(new WebSocketEvent(webSocketMessage) {});
  }
}
