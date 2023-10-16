package org.cardanofoundation.explorer.api.job;

import lombok.RequiredArgsConstructor;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import org.cardanofoundation.explorer.api.event.websocket.WebSocketEvent;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;

@Component
@RequiredArgsConstructor
public class CurrentBlockScheduler {

  private final BlockRepository blockRepository;
  private final ApplicationEventPublisher applicationEventPublisher;
  private final WebSocketService webSocketService;

  @Scheduled(fixedDelayString = "20000")
  public void sendCurrentBlockInfo() {
    Block currentBlock = blockRepository.findCurrentBlockById();

    BlockSyncMessage blockSyncMessage = BlockSyncMessage.builder()
        .lastBlockHash(currentBlock.getHash())
        .lastBlockNo(currentBlock.getBlockNo())
        .hasTx(currentBlock.getTxCount() > 0)
        .build();
    WebSocketMessage webSocketMessage =
        webSocketService.getBatchBlockInfoMessage(blockSyncMessage);
    applicationEventPublisher.publishEvent(new WebSocketEvent(webSocketMessage) {});
  }
}
