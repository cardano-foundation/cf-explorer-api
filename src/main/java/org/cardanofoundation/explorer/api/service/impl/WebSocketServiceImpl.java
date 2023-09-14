package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncInfo;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncMessage;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.MarketDataService;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
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
            .hashTx(block.getTxCount() > 0)
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
            .hashTx(blockSyncMessage.isHashTx())
            .epochSummary(epochService.getCurrentEpochSummary())
            .build();
    return WebSocketMessage.builder()
        .eventType(WebSocketEventType.BLOCK)
        .payload(blockSyncInfo)
        .build();
  }
}
