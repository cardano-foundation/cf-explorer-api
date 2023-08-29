package org.cardanofoundation.explorer.api.event;

import org.springframework.context.ApplicationEvent;

public class BlockSyncEvent extends ApplicationEvent {

  /**
   * Create a new {@code ApplicationEvent} with its {@link #getTimestamp() timestamp} set to {@link
   * System#currentTimeMillis()}.
   *
   * @param blockNo the object on which the event initially occurred or with which the event is
   *     associated (never {@code null})
   */
  public BlockSyncEvent(Long blockNo) {
    super(blockNo);
  }
}
