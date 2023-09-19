package org.cardanofoundation.explorer.api.config;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.event.websocket.WebSocketEventHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

@Configuration
@EnableWebSocket
@RequiredArgsConstructor
public class WebSocketConfig implements WebSocketConfigurer {

  private final WebSocketEventHandler webSocketEventHandler;

  /**
   * Register {@link WebSocketHandler WebSocketHandlers} including SockJS fallback options if
   * desired.
   *
   * @param registry
   */
  @Override
  public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
    registry.addHandler(webSocketEventHandler, "/ws").setAllowedOrigins("*");
  }
}
