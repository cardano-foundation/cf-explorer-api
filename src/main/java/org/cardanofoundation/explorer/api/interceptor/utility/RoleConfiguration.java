package org.cardanofoundation.explorer.api.interceptor.utility;

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
    String authData = FileUtils.readFile(path);
    ObjectMapper objectMapper = new ObjectMapper();
    return objectMapper.readValue(authData,
        RoleFilterMapper.class);
  }

}
