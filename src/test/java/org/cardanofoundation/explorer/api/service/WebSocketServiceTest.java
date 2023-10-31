package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.math.BigInteger;
import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;
import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncInfo;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.service.impl.WebSocketServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class WebSocketServiceTest {

  @Mock MarketDataService marketDataService;
  @Mock EpochService epochService;
  @Mock BlockRepository blockRepository;

  @InjectMocks WebSocketServiceImpl webSocketServiceImpl;

  @Test
  void getMarketDataMessage_withCurrencyUSD_shouldReturnResponse() {
    when(marketDataService.getMarketData("usd")).thenReturn("Payload");
    WebSocketMessage expected =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.CURRENT_PRICE_USD)
            .payload("Payload")
            .build();
    WebSocketMessage actual = webSocketServiceImpl.getMarketDataMessage("usd");
    Assertions.assertEquals(expected.getEventType(), actual.getEventType());
    Assertions.assertEquals(expected.getPayload(), actual.getPayload());
  }

  @Test
  void getMarketDataMessage_withCurrencyBTC_shouldReturnResponse() {
    when(marketDataService.getMarketData("btc")).thenReturn("Payload");
    WebSocketMessage expected =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.CURRENT_PRICE_BTC)
            .payload("Payload")
            .build();
    WebSocketMessage actual = webSocketServiceImpl.getMarketDataMessage("btc");
    Assertions.assertEquals(expected.getEventType(), actual.getEventType());
    Assertions.assertEquals(
        JsonUtil.getPrettyJson(expected.getPayload()), JsonUtil.getPrettyJson(actual.getPayload()));
  }

  @Test
  void getCurrentBlockInfoMessage_shouldReturnResponse() {
    Block blockPrepared = Block.builder().blockNo(1L).hash("hash").txCount(100L).build();
    EpochSummary epochSummaryPrepared =
        EpochSummary.builder()
            .no(1)
            .totalSlot(100)
            .slot(1)
            .circulatingSupply(BigInteger.TEN)
            .build();

    BlockSyncInfo blockSyncInfo =
        BlockSyncInfo.builder()
            .blockNo(blockPrepared.getBlockNo())
            .blockHash(blockPrepared.getHash())
            .hasTx(blockPrepared.getTxCount() > 0)
            .epochSummary(epochSummaryPrepared)
            .build();
    when(blockRepository.findCurrentBlockById()).thenReturn(blockPrepared);
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummaryPrepared);

    WebSocketMessage expected =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.BLOCK)
            .payload(blockSyncInfo)
            .build();
    WebSocketMessage actual = webSocketServiceImpl.getCurrentBlockInfoMessage();
    Assertions.assertEquals(expected.getEventType(), actual.getEventType());
    Assertions.assertEquals(
        JsonUtil.getPrettyJson(expected.getPayload()), JsonUtil.getPrettyJson(actual.getPayload()));
  }

  @Test
  void getBatchBlockInfoMessage_shouldReturnResponse() {
    BlockSyncMessage blockSyncMessage =
        BlockSyncMessage.builder().lastBlockNo(1L).hasTx(true).lastBlockHash("hash").build();
    EpochSummary epochSummaryPrepared =
        EpochSummary.builder()
            .no(1)
            .totalSlot(100)
            .slot(1)
            .circulatingSupply(BigInteger.TEN)
            .build();
    BlockSyncInfo blockSyncInfo =
        BlockSyncInfo.builder()
            .blockNo(blockSyncMessage.getLastBlockNo())
            .blockHash(blockSyncMessage.getLastBlockHash())
            .hasTx(blockSyncMessage.isHasTx())
            .epochSummary(epochSummaryPrepared)
            .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummaryPrepared);

    WebSocketMessage expected =
        WebSocketMessage.builder()
            .eventType(WebSocketEventType.BLOCK)
            .payload(blockSyncInfo)
            .build();
    WebSocketMessage actual = webSocketServiceImpl.getBatchBlockInfoMessage(blockSyncMessage);
    Assertions.assertEquals(expected.getEventType(), actual.getEventType());
    Assertions.assertEquals(
        JsonUtil.getPrettyJson(expected.getPayload()), JsonUtil.getPrettyJson(actual.getPayload()));
  }
}
