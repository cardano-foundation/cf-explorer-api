package org.cardanofoundation.explorer.api.event.websocket;

import org.springframework.context.ApplicationEvent;

public class WebSocketEvent extends ApplicationEvent {

  /**
   * Create a new {@code ApplicationEvent} with its {@link #getTimestamp() timestamp} set to {@link
   * System#currentTimeMillis()}.
   *
   * @param webSocketMessage the object on which the event initially occurred or with which the
   *     event is associated (never {@code null})
   */
  public WebSocketEvent(WebSocketMessage webSocketMessage) {
    super(webSocketMessage);
  }
}
