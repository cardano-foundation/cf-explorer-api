package org.cardanofoundation.explorer.api.event.websocket;

import java.util.concurrent.ConcurrentLinkedQueue;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.service.WebSocketService;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

@Log4j2
@Component
@RequiredArgsConstructor
public class WebSocketEventHandler extends TextWebSocketHandler {

  private final ConcurrentLinkedQueue<WebSocketSession> sessions = new ConcurrentLinkedQueue<>();
  private final WebSocketService webSocketService;

  /**
   * Invoked after WebSocket negotiation has succeeded and the WebSocket connection is opened and
   * ready for use.
   *
   * @param session
   * @throws Exception this method can handle or propagate exceptions; see class-level Javadoc for
   *     details.
   */
  @Override
  public void afterConnectionEstablished(WebSocketSession session) throws Exception {
    sessions.add(session);
    session.sendMessage(
        new TextMessage(JsonUtil.getPrettyJson(webSocketService.getCurrentBlockInfoMessage())));
    session.sendMessage(
        new TextMessage(JsonUtil.getPrettyJson(webSocketService.getMarketDataMessage("btc"))));
    session.sendMessage(
        new TextMessage(JsonUtil.getPrettyJson(webSocketService.getMarketDataMessage("usd"))));

    super.afterConnectionEstablished(session);
  }

  /**
   * Invoked after the WebSocket connection has been closed by either side, or after a transport
   * error has occurred. Although the session may technically still be open, depending on the
   * underlying implementation, sending messages at this point is discouraged and most likely will
   * not succeed.
   *
   * @param session
   * @param closeStatus
   * @throws Exception this method can handle or propagate exceptions; see class-level Javadoc for
   *     details.
   */
  @Override
  public void afterConnectionClosed(WebSocketSession session, CloseStatus closeStatus)
      throws Exception {
    sessions.remove(session);
    super.afterConnectionClosed(session, closeStatus);
  }

  @EventListener
  public void publish(WebSocketEvent webSocketEvent) {
    WebSocketMessage<String> message =
        new TextMessage(JsonUtil.getPrettyJson(webSocketEvent.getSource()));
    sessions.forEach(
        s -> {
          try {
            s.sendMessage(message);
          } catch (Exception e) {
            log.error("Error sending message", e);
          }
        });
  }
}
