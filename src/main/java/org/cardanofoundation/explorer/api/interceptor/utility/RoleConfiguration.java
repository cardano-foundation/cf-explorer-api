package org.cardanofoundation.explorer.api.interceptor.utility;

import java.io.FileNotFoundException;
import java.io.IOException;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;
import org.springframework.util.ResourceUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.ledgersync.common.util.FileUtils;

@Component
@Getter
@Setter
@Log4j2
public class RoleConfiguration {

  @Value("${keycloak.role.path}")
  private String path;

  @Bean
  public RoleFilterMapper getRoleConfiguration() throws IOException {
    String content = getStringContent();
    ObjectMapper objectMapper = new ObjectMapper();
    return objectMapper.readValue(content,
        RoleFilterMapper.class);
  }

  private String getStringContent() {
    try {
      if (path.startsWith("classpath:")) {
        return FileUtils.readFileFromClasspath(path.substring(10));
      } else {
        return FileUtils.readFileFromUrl(ResourceUtils.getFile(path).getAbsolutePath());
      }
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      log.error("exception {} with url {}", e.getMessage(), path);
      throw new IllegalStateException("can't load file " + path);
    }
  }

}
