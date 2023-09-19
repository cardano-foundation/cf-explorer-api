package org.cardanofoundation.explorer.api.event.websocket;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.common.enumeration.WebSocketEventType;

@Builder
@Getter
@Setter
public class WebSocketMessage {
  private WebSocketEventType eventType;
  private Object payload;
}
