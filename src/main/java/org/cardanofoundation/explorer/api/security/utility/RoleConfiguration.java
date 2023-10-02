package org.cardanofoundation.explorer.api.security.utility;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.mapper.RoleFilterMapper;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

@Component
@ConfigurationProperties("keycloak.role")
@Getter
@Setter
public class RoleConfiguration {
    private String path;

    public RoleFilterMapper getRoleConfiguration() throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        HashMap<String ,RoleFilterMapper> roleConfigs = new HashMap<>();
        RoleFilterMapper roleConfigJson = objectMapper.readValue(new ClassPathResource(path).getFile(), RoleFilterMapper.class);
        return roleConfigJson;
    }

}
