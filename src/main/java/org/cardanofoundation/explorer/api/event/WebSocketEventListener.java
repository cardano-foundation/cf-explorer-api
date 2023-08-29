package org.cardanofoundation.explorer.api.event;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.enumeration.DestinationPrefix;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.simp.SimpMessageHeaderAccessor;
import org.springframework.messaging.simp.SimpMessageSendingOperations;
import org.springframework.messaging.simp.SimpMessageType;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.messaging.SessionConnectedEvent;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;
import org.springframework.web.socket.messaging.SessionSubscribeEvent;
import org.springframework.web.socket.messaging.SessionUnsubscribeEvent;

@Service
@Log4j2
@RequiredArgsConstructor
public class WebSocketEventListener {

  /** Stores the session ID, destination topic mapping for each subscription */
  private final ConcurrentHashMap<String, Set<DestinationPrefix>> subscriptions =
      new ConcurrentHashMap<>();

  /** The messaging template to send messages to the client */
  private final SimpMessageSendingOperations messagingTemplate;

  /**
   * Handles the WebSocket connection event, Retrieves the session ID, stores it along with
   * associated subscriptions
   *
   * @param sessionConnectedEvent
   */
  @EventListener
  public void handleWebSocketConnectListener(SessionConnectedEvent sessionConnectedEvent) {
    StompHeaderAccessor headerAccessor =
        StompHeaderAccessor.wrap(sessionConnectedEvent.getMessage());
    String sessionId = headerAccessor.getSessionId();
    subscriptions.putIfAbsent(sessionId, new HashSet<>());
    log.info("Web Socket Connected with session id: {}", sessionId);
  }

  /**
   * Handles the WebSocket disconnection event, Retrieves the session ID, removes it along with
   * associated subscriptions
   *
   * @param sessionDisconnectEvent
   */
  @EventListener
  public void handleWebSocketDisconnectListener(SessionDisconnectEvent sessionDisconnectEvent) {
    StompHeaderAccessor headerAccessor =
        StompHeaderAccessor.wrap(sessionDisconnectEvent.getMessage());
    String sessionId = headerAccessor.getSessionId();
    if (sessionId != null) {
      subscriptions.remove(sessionId);
    }
    log.info("Web Socket Disconnected with session id: {}", sessionId);
  }

  /**
   * Handles the WebSocket subscription event, Add the destination topic of the subscription with
   * the session ID
   *
   * @param sessionSubscribeEvent
   */
  @EventListener
  public void handleWebSocketSubscribeListener(SessionSubscribeEvent sessionSubscribeEvent) {
    StompHeaderAccessor headerAccessor =
        StompHeaderAccessor.wrap(sessionSubscribeEvent.getMessage());
    String sessionId = headerAccessor.getSessionId();
    DestinationPrefix destination = DestinationPrefix.from(headerAccessor.getDestination());
    Set<DestinationPrefix> destinationSet =
        subscriptions.computeIfAbsent(sessionId, unused -> new HashSet<>());
    destinationSet.add(destination);
    log.info("Web Socket subscribed with session id: {}, destination: {}", sessionId, destination);
  }

  /**
   * Handles the WebSocket subscription event, Remove the destination topic of the subscription with
   * the session ID
   *
   * @param sessionUnsubscribeEvent
   */
  @EventListener
  public void handleWebSocketUnsubscribeListener(SessionUnsubscribeEvent sessionUnsubscribeEvent) {
    StompHeaderAccessor headerAccessor =
        StompHeaderAccessor.wrap(sessionUnsubscribeEvent.getMessage());
    String sessionId = headerAccessor.getSessionId();
    DestinationPrefix destination = DestinationPrefix.from(headerAccessor.getDestination());
    Set<DestinationPrefix> destinationSet =
        subscriptions.computeIfAbsent(sessionId, unused -> new HashSet<>());
    destinationSet.remove(destination);
    log.info(
        "Web Socket unsubscribed with session id: {}, destination: {}", sessionId, destination);
  }

  /**
   * Creates the message headers for the given session ID
   *
   * @param sessionId The session ID
   * @return The message headers
   */
  private MessageHeaders createHeaders(String sessionId) {
    SimpMessageHeaderAccessor headerAccessor =
        SimpMessageHeaderAccessor.create(SimpMessageType.MESSAGE);
    headerAccessor.setSessionId(sessionId);
    headerAccessor.setLeaveMutable(true);
    return headerAccessor.getMessageHeaders();
  }

  /**
   * Handles the block sync event, Publishes the latest block no to all the subscribers
   *
   * @param blockSyncEvent
   */
  @EventListener
  private void publish(BlockSyncEvent blockSyncEvent) {
    long startTime = System.currentTimeMillis();
    log.info(
        "Publishing block sync event to {} subscribers. Block no: {}",
        subscriptions.size(),
        blockSyncEvent.getSource());
    subscriptions.forEach(
        (sessionId, destinations) ->
            destinations.forEach(
                destination ->
                    messagingTemplate.convertAndSendToUser(
                        sessionId,
                        destination.getValue(),
                        blockSyncEvent.getSource(),
                        createHeaders(sessionId))));
    log.info(
        "Published block sync event to {} subscribers. Block no: {}. Time taken: {} ms",
        subscriptions.size(),
        blockSyncEvent.getSource(),
        System.currentTimeMillis() - startTime);
  }
}
