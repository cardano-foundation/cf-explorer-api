package org.cardanofoundation.explorer.api.security.utility;

import java.io.IOException;

import lombok.Getter;
import lombok.Setter;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.mapper.RoleFilterMapper;

@Component
@ConfigurationProperties("keycloak.role")
@Getter
@Setter
public class RoleConfiguration {

  private String path;

  @Bean
  public RoleFilterMapper getRoleConfiguration() throws IOException {
    ObjectMapper objectMapper = new ObjectMapper();
    return objectMapper.readValue(new ClassPathResource(path).getFile(),
        RoleFilterMapper.class);
  }

}
