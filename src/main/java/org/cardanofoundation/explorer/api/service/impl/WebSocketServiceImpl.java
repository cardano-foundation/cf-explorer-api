package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncInfo;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;

@Service
@RequiredArgsConstructor
@Log4j2
public class WebSocketServiceImpl implements WebSocketService {

  private final MarketDataService marketDataService;
  private final EpochService epochService;
  private final BlockRepository blockRepository;

  @Override
  public WebSocketMessage getMarketDataMessage(String currency) {
    Object payload = marketDataService.getMarketData(currency);
    WebSocketMessage webSocketMessage = WebSocketMessage.builder().payload(payload).build();
    if (currency.equals("usd")) {
      webSocketMessage.setEventType(WebSocketEventType.CURRENT_PRICE_USD);
    } else if (currency.equals("btc")) {
      webSocketMessage.setEventType(WebSocketEventType.CURRENT_PRICE_BTC);
    }
    return webSocketMessage;
  }

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
