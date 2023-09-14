package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.event.blocksync.BlockSyncMessage;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketMessage;

public interface WebSocketService {

  WebSocketMessage getMarketDataMessage(String currency);

  WebSocketMessage getCurrentBlockInfoMessage();

  WebSocketMessage getBatchBlockInfoMessage(BlockSyncMessage blockSyncMessage);
}
