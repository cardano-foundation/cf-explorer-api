package org.cardanofoundation.explorer.api.util;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.log4j.Log4j2;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Log4j2
public class FileUtil {

  public static String readFile(String url) {
    StringBuilder content = new StringBuilder();
    try (BufferedInputStream fileGenesis = new BufferedInputStream(new FileInputStream(url))) {
      byte[] bytes = new byte[500];
      while (fileGenesis.available() != 0) {
        fileGenesis.read(bytes);
        content.append(new String(bytes));
      }
    } catch (IOException ex) {
      log.error("Error: " + ex.getMessage());
    }
    return content.toString();
  }
}
