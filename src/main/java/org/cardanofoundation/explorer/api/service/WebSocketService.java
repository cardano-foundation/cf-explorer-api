package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;
import org.cardanofoundation.ledgersync.common.redis.BlockSyncMessage;

public interface WebSocketService {

  WebSocketMessage getMarketDataMessage(String currency);

  WebSocketMessage getCurrentBlockInfoMessage();

  WebSocketMessage getBatchBlockInfoMessage(BlockSyncMessage blockSyncMessage);
}
