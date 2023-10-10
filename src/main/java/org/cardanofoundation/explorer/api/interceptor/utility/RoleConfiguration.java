package org.cardanofoundation.explorer.api.interceptor.utility;

import java.io.BufferedInputStream;
import java.io.IOException;

import lombok.Getter;
import lombok.Setter;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.ledgersync.common.util.FileUtils;

@Component
@Getter
@Setter
public class RoleConfiguration {

  @Value("${keycloak.role.path}")
  private String path;

  @Bean
  public RoleFilterMapper getRoleConfiguration() throws IOException {
    StringBuilder content = new StringBuilder();
    try (BufferedInputStream file = new BufferedInputStream(
        FileUtils.class.getClassLoader().getResourceAsStream(path))) {
      byte[] bytes = new byte[500];
      while (file.available() != 0) {
        file.read(bytes);
        content.append(new String(bytes));
      }
    } catch (IOException exception) {
      throw new RuntimeException("Can not read data of configuration file");
    }
    ObjectMapper objectMapper = new ObjectMapper();
    return objectMapper.readValue(content.toString(),
        RoleFilterMapper.class);
  }

}
