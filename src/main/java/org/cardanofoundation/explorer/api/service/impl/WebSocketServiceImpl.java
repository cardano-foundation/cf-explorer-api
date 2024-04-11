package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncInfo;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncMessage;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;

@Service
@RequiredArgsConstructor
@Log4j2
public class WebSocketServiceImpl implements WebSocketService {

  private final EpochService epochService;
  private final BlockRepository blockRepository;

  @Override
  public WebSocketMessage getCurrentBlockInfoMessage() {
    Block block = blockRepository.findCurrentBlockById();
    BlockSyncInfo blockSyncInfo =
        BlockSyncInfo.builder()
            .blockNo(block.getBlockNo())
            .blockHash(block.getHash())
            .hasTx(block.getTxCount() > 0)
            .epochSummary(epochService.getCurrentEpochSummary())
            .build();
    return WebSocketMessage.builder()
        .eventType(WebSocketEventType.BLOCK)
        .payload(blockSyncInfo)
        .build();
  }

  @Override
  public WebSocketMessage getBatchBlockInfoMessage(BlockSyncMessage blockSyncMessage) {
    BlockSyncInfo blockSyncInfo =
        BlockSyncInfo.builder()
            .blockNo(blockSyncMessage.getLastBlockNo())
            .blockHash(blockSyncMessage.getLastBlockHash())
            .hasTx(blockSyncMessage.isHasTx())
            .epochSummary(epochService.getCurrentEpochSummary())
            .build();
    return WebSocketMessage.builder()
        .eventType(WebSocketEventType.BLOCK)
        .payload(blockSyncInfo)
        .build();
  }
}
