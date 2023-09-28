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

@Component
@ConfigurationProperties("keycloak.role")
@Getter
@Setter
public class RoleConfiguration {
    private String path;

    public HashMap<String ,RoleFilterMapper> getRoleConfiguration() throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        HashMap<String ,RoleFilterMapper> roleConfigs = new HashMap<>();
        JsonNode roleConfigJson = objectMapper.readValue(new ClassPathResource(path).getFile(), JsonNode.class).get("roleConfig");

        if (roleConfigJson.isArray()) {
            for(JsonNode role: roleConfigJson){
                RoleFilterMapper obj = RoleFilterMapper.builder()
                        .role(role.get("role").asText())
                        .apiUrl(role.get("apiUrl").asText())
                        .build();

                String key = role.get("role").asText() + role.get("apiUrl").asText();

                roleConfigs.put(key, obj);
            }
        }

        return roleConfigs;
    }


}
