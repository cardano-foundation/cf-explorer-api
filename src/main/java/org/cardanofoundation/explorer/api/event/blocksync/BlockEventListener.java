package org.cardanofoundation.explorer.api.event.blocksync;

import java.sql.Connection;
import java.util.function.Consumer;

import javax.sql.DataSource;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import org.postgresql.PGConnection;
import org.postgresql.PGNotification;

@Slf4j
@RequiredArgsConstructor
@Component
public class BlockEventListener {

  private static final String NOTIFCATIONS = "new_block";
  private final DataSource dataSource;

  @Async
  public void listenForNotifications(Consumer<PGNotification> consumer) {
    while (true) {
      try (Connection c = dataSource.getConnection()) {
        PGConnection pgconn = c.unwrap(PGConnection.class);
        c.createStatement().execute("LISTEN " + NOTIFCATIONS);
        c.commit();
        log.info(
            "Connection established: Listening for notifications on channel: [{}]", NOTIFCATIONS);
        while (!Thread.currentThread().isInterrupted()) {
          PGNotification[] nts = pgconn.getNotifications(10000);
          if (nts == null) {
            continue;
          }

          for (PGNotification nt : nts) {
            consumer.accept(nt);
          }
        }
      } catch (Exception e) {
        log.warn("Error occurred while listening for notifications, attempting to reconnect...", e);
      }
    }
  }
}
